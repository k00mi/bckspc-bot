module Redmine (initRedmine) where

import           Control.Applicative
import           Control.Concurrent         (withMVar, MVar, forkIO,
                                            threadDelay)
import           Control.Exception          (handle, IOException)
import           Data.Aeson                 (eitherDecode, (.:), (.:?),
                                            Value(..), Object, encode)
import           Data.Aeson.Types           (parseEither, FromJSON, Parser)
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy       as LBS
import           Data.Foldable              (for_)
import           Data.Monoid
import           Data.Text                  (Text, pack)
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Traversable           (traverse)
import qualified Data.Vector                as V
import           Network.HTTP.Client        (applyBasicAuth)
import           Network.SimpleIRC
import           System.Directory           (renameFile)
import           System.Posix.Syslog

import Config
import Utils

option :: String -> Redmine -> Redmine
option opt rm = rm { rmURL = rmURL rm ++ opt ++ "&" }

closed :: Redmine -> Redmine
closed = option "status_id=closed"

lastUpdatedFirst :: Redmine -> Redmine
lastUpdatedFirst = option "sort=updated_on:desc"

updatedAfter :: String -> Redmine -> Redmine
updatedAfter date = option ("updated_on=%3E%3D" ++ date) . lastUpdatedFirst

latest :: Redmine -> Redmine
latest = option "limit=1" . lastUpdatedFirst

getRedmine :: Redmine -> IO LBS.ByteString
getRedmine rm = myGetURL (rmURL rm)
  where
    myGetURL =
      case (,) <$> rmUser rm <*> rmPassword rm of
        Just (user, pass) ->
          getURLTransformRQ (applyBasicAuth (BSC.pack user) (BSC.pack pass))
        Nothing -> getURL

getRedmineJSON :: FromJSON a
               => Redmine -> (a -> Parser b) -> IO (Either String b)
getRedmineJSON = getJSONWith . getRedmine

getLatestClosed :: Redmine -> IO (Either String String)
getLatestClosed rm = getRedmineJSON (latest $ closed rm) parseDate

parseDate :: FromJSON a => Object -> Parser a
parseDate o = do
    is <- o .: "issues"
    i <- case is of
           [] -> fail "No closed issue found."
           i:_ -> pure i
    i .: "updated_on"

getClosedByAfter :: Redmine -> String
                 -> IO (Either String (V.Vector (Int, Maybe Text), String))
getClosedByAfter rm date =
    getRedmineJSON (updatedAfter date $ closed rm) $ \o ->
      (,) <$> parseAssignees o <*> parseDate o
  where
    parseAssignees o = do
      assignees <- o .: "issues" >>=
                    traverse (\i -> (,) <$> i .: "id" <*> assignee i)
      -- the API can only filter >= date, so we receive one update again
      return $ V.take (V.length assignees - 1) assignees

    assignee o = o .:? "assigned_to" >>= traverse (.: "name")


initRedmine :: Config -> MIrc -> MVar String -> IO ()
initRedmine cfg serv karmaVar = for_ (redmine cfg) $ \rm -> forkIO $ do
    eitherDate <- getLatestClosed rm
    case eitherDate of
      Left err -> syslog Error $ "[redmineLoop] getLatestClosed: " ++ err
      Right date -> redmineLoop rm date
  where
    redmineLoop rm date = do
      threadDelay $ 10^6 * 60 * 60 -- 1 hour
      eitherNew <- getClosedByAfter rm date
      newDate <- case eitherNew of
        Left err -> do
          syslog Warning $ "[redmineLoop] getClosedByAfter: " ++ err
          return date
        Right (closedTasks, newDate) -> withMVar karmaVar $ \karmaFile -> do
          handle
            (\e -> syslog Warning $ "[redmineLoop] " ++ show (e :: IOException))
            (do
              json <- LBS.readFile karmaFile
              case eitherDecode json of
                Left err -> syslog Error $
                  "[redmineLoop] eitherDecode: " ++ err
                Right obj -> do
                  let obj' = V.foldl' maybeInc obj closedTasks
                      maybeInc o (_, Just nick) =
                        snd $ increment (sanitize nick) o
                      maybeInc o _              = o
                      newFile = karmaFile ++ ".new"
                  LBS.writeFile newFile (encode obj')
                  renameFile newFile karmaFile
                  for_ closedTasks $ \(task, mAssignee) ->
                    for_ mAssignee $ \assignee ->
                      sendMsg serv (BSC.pack $ channel cfg) $
                        encodeUtf8 assignee
                          <> " closed ticket #"
                          <> BSC.pack (show task)
            )
          return newDate
      redmineLoop rm newDate

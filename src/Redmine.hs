module Redmine (initRedmine) where

import           Control.Applicative
import           Control.Concurrent         (withMVar, MVar, forkIO,
                                            threadDelay)
import           Control.Exception          (handle, IOException)
import           Data.Aeson                 (eitherDecode, (.:), (.:?),
                                            Object, encode)
import           Data.Aeson.Types           (FromJSON, Parser)
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy       as LBS
import           Data.Foldable              (for_)
import           Data.Monoid
import           Data.Text                  (Text)
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Traversable           (traverse)
import           Network.HTTP.Client        (applyBasicAuth)
import           Network.SimpleIRC
import           System.Directory           (renameFile)
import           System.Posix.Syslog

import Config (Config(channel, redmine), Redmine(..))
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
                 -> IO (Either String ([(Int, Text)], String))
getClosedByAfter rm date =
    getRedmineJSON (updatedAfter date $ closed rm) $ \o ->
      (,) <$> parseAssignees o <*> parseDate o
  where
    parseAssignees o = do
      assignees <- o .: "issues" >>=
                    traverse
                      (\i -> (,,) <$> i .: "id"
                                  <*> assignee i
                                  <*> i .: "updated_on")
      return [ (task, asgne)
             | (task, Just asgne, date') <- assignees
             -- the API can only filter >= date, so we receive one update again
             , date' /= date
             ]

    assignee o = o .:? "assigned_to" >>= traverse (.: "name")


initRedmine :: Config -> MIrc -> MVar String -> IO ()
initRedmine cfg serv karmaVar = for_ (redmine cfg) $ \rm -> forkIO $ do
    eitherDate <- getLatestClosed rm
    case eitherDate of
      Left err -> syslog Error $ "[redmineLoop] getLatestClosed: " ++ err
      Right date -> redmineLoop rm date
  where
    redmineLoop rm date = do
      threadDelay $ 10^6 * 60 * rmInterval rm
      eitherNew <- getClosedByAfter rm date
      newDate <- case eitherNew of
        Left err -> do
          syslog Warning $ "[redmineLoop] getClosedByAfter: " ++ err
          return date
        Right (closedTasks, newDate) -> withMVar karmaVar $ \karmaFile -> do
          giveKarma closedTasks karmaFile
          return newDate
      redmineLoop rm newDate

    giveKarma :: [(Int, Text)] -> String -> IO ()
    giveKarma closedTasks karmaFile =
      handle
        (\e -> syslog Warning $ "[redmineLoop] " ++ show (e :: IOException))
        (do updateKarmaFile karmaFile $ \obj ->
                let inc o (_, nick) = snd $ increment (sanitize nick) o
                in foldl inc obj closedTasks
            for_ closedTasks $ \(task, assignee) ->
              sendMsg serv (BSC.pack $ channel cfg) $
                encodeUtf8 assignee
                  <> " closed ticket #"
                  <> BSC.pack (show task)
        )

updateKarmaFile :: String -> (Object -> Object) -> IO ()
updateKarmaFile file f = do
  json <- LBS.readFile file
  case eitherDecode json of
    Left err -> syslog Error $
      "[redmineLoop] eitherDecode: " ++ err
    Right obj -> do
      let newFile = file ++ ".new"
      LBS.writeFile newFile (encode $ f obj)
      renameFile newFile file

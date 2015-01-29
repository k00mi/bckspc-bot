module Redmine (initRedmine) where

import           Control.Applicative
import           Control.Concurrent         (withMVar, MVar, forkIO,
                                            threadDelay)
import           Control.Exception          (handle, IOException)
import           Data.Aeson                 (eitherDecode, (.:), Object, encode)
import           Data.Aeson.Types           (FromJSON, Parser)
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy       as LBS
import           Data.Foldable              (for_)
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Traversable           (traverse)
import           Network.HTTP.Client        (applyBasicAuth)
import           Network.SimpleIRC
import           System.Directory           (renameFile)
import           System.Posix.Syslog

import Config (Config(channel, redmine), Redmine(..))
import Utils

data Issue = Issue
           { issueID :: Int
           , issueAssignee :: Text
           , issueSubject :: Text
           }

project :: Redmine -> Redmine
project rm =
    rm { rmURL = rmURL rm ++ "/projects/" ++ rmProject rm ++ "/issues.json?" }

option :: String -> Redmine -> Redmine
option opt rm = rm { rmURL = rmURL rm ++ opt ++ "&" }

closed :: Redmine -> Redmine
closed = option "status_id=4"

lastUpdatedFirst :: Redmine -> Redmine
lastUpdatedFirst = option "sort=closed_on:desc"

closedAfter :: String -> Redmine -> Redmine
closedAfter date = option ("closed_on=%3E%3D" ++ date) . lastUpdatedFirst

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
getLatestClosed rm = getRedmineJSON (latest $ closed $ project rm) parseDate

parseDate :: FromJSON a => Object -> Parser a
parseDate o = do
    is <- o .: "issues"
    i <- case is of
           [] -> fail "No closed issue found."
           i:_ -> pure i
    i .: "closed_on"

getClosedByAfter :: Redmine -> String
                 -> IO (Either String ([Issue], String))
getClosedByAfter rm date =
    getRedmineJSON (closedAfter date $ closed $ project rm) $ \o ->
      (,) <$> parseIssue o <*> parseDate o
  where
    parseIssue o = do
      assignees <- o .: "issues" >>=
                    traverse
                      (\i -> optional $ do
                                issue <- Issue <$> i .: "id"
                                               <*> assignee i
                                               <*> i .: "subject"
                                closedOn <- i .: "closed_on"
                                return (issue, closedOn))
      return [ issue
             | Just (issue, date') <- assignees
             -- the API can only filter >= date, so we receive one update again
             , date' /= date
             ]

    assignee o = o .: "assigned_to" >>= (.: "name")


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
          giveKarma rm closedTasks karmaFile
          return newDate
      redmineLoop rm newDate

    giveKarma :: Redmine -> [Issue] -> String -> IO ()
    giveKarma rm closedTasks karmaFile =
      handle
        (\e -> syslog Warning $ "[redmineLoop] " ++ show (e :: IOException))
        (do updateKarmaFile karmaFile $ \obj ->
                let inc o i = snd $ increment (sanitize (issueAssignee i)) o
                in foldl inc obj closedTasks
            for_ closedTasks $ \i ->
              sendMsg serv (BSC.pack $ channel cfg) $ encodeUtf8 $
                issueAssignee i <> " completed ticket \"" <> issueSubject i
                <> "\" <" <> T.pack (rmURL rm) <> "/issues/"
                <> T.pack (show (issueID i)) <> ">"
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

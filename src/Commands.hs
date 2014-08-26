{-# LANGUAGE OverloadedStrings, TupleSections, MultiWayIf #-}

module Commands
  ( commands
  , CommandMap
  , addKarma
  ) where

import           Control.Monad
import           Control.Applicative
import           Control.Concurrent         (withMVar, readMVar)
import           Data.Monoid                ((<>))
import           Data.Maybe
import           Data.Foldable              (for_)
import           Data.List                  (sortBy)
import           Data.Ord                   (comparing, Down(..))
import           Text.Read                  (readMaybe)
import qualified Data.ByteString.Lazy       as BL
import           Data.Text                  (Text, pack, unpack)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Data.Text.Read             (decimal)
import qualified Data.Map                   as M
import qualified Data.HashMap.Strict        as HM
import           Control.Concurrent         (forkIO, threadDelay)
import           System.IO.Error            (tryIOError)
import           System.Directory           (renameFile)
import           Network.SimpleIRC
import           Data.Aeson                 hiding (Error)
import           Data.Aeson.Types           hiding (Error)
import           System.Posix.Syslog

import EventEnv
import Utils


type CommandMap = M.Map Text ([Text] -> EventEnv ())

commands :: CommandMap
commands = M.fromList [ ("echo", echo)
                      , ("help", help)
                      , ("inspace", inspace)
                      , ("pizza", pizza)
                      , ("karma", karma)
                      , ("karmatop", karmatop)
                      , ("alarm", alarm)
                      , ("i", inspace)
                      , ("p", pizza)
                      , ("a", alarm)
                      , ("k", karma)
                      ]


echo :: [Text] -> EventEnv ()
echo = respond . T.unwords


help :: [Text] -> EventEnv ()
help _ = respondNick "see https://github.com/k00mi/bckspc-bot"

inspace :: [Text] -> EventEnv ()
inspace _ = do
    url <- asks statusUrl
    res <- lift $ getMembersPresent url
    response <- case res of
            Left err -> do
              lift . syslog Warning $ "inspace: Error fetching JSON: " ++ err
              pure $ "Error retrieving status information"
            Right (num, nicks)
                | num == (0 :: Int) -> pure $ "Backspace is empty"
                | otherwise         -> pure $ pack (show num)
                                            <> " members present: "
                                            <> T.intercalate ", "
                                                (map avoidHighlighting nicks)
    respondNick response


pizza :: [Text] -> EventEnv ()
pizza args =
    case args of
      []    -> notifyIn $ mins 15
      arg:_ -> either (const parseErr) notifyIn $ getTime arg
  where
    notifyIn t = do
        sendTimeUp <- asIO $ do
            respondNick "Time is up!"
            pizzaTopic <- asks (pizzaTopic . mqttEnv)
            publish pizzaTopic ""
        lift . forkIO $ do
            threadDelay t
            sendTimeUp
        respondNick "I won't forget it!"

    parseErr = respondNick "Could not parse duration"

    getTime str = do
        (num, rest) <- decimal str
        case rest of
          "s" -> Right $ secs num
          "m" -> Right $ mins num
          "h" -> Right $ hours num
          ""  -> Right $ mins num
          _   -> Left "invalid suffix"

    secs :: Int -> Int
    secs  x = x * 10^6
    mins  x = x * secs 60
    hours x = x * mins 60


addKarma :: Text -> EventEnv ()
addKarma nick = do
    message <- asks msg
    let sender = sanitize . decodeUtf8 . fromJust . mNick $ message
        nick'  = sanitize nick
    if | isPM message -> respondNick "You can only give karma in the channel"
       | nick' == sender -> respondNick "You can't give yourself karma"
       | otherwise -> onKarmaFile $ \o ->
          let (n, o') = increment nick' o
              now = pack (show n)
          in Just o' <$ respond (nick <> " now has " <> now <> " karma!")


karma :: [Text] -> EventEnv ()
karma nicks = onKarmaFile $ \obj -> do
    sender <- asks $ fromJust . mNick . msg
    let resp = flip parseMaybe obj $ \o ->
          if not $ null nicks
            then fmap toString $
                 forM nicks $ \n ->
                    let nick = sanitize n
                    in (nick,) <$> o .:? nick .!= 0
            else do
              score <- o .:? sanitize (decodeUtf8 sender)
              return $ maybe
                "You have no karma yet."
                (\n -> "You have " <> pack (show (n :: Int)) <> " karma.")
                score
    maybe
      (respondNick "Error parsing karma file")
      respondNick
      resp
    pure Nothing


karmatop :: [Text] -> EventEnv ()
karmatop nums =
    if n > 5
      then respondNick "At most 5 nicks will be shown"
      else onKarmaFile $
          (pure Nothing <*)
          . respondNick
          . toString
          . take n
          . sortBy (comparing $ Down . snd)
          . map (\(name, Number x) -> (name, truncate x))
          . HM.toList
  where
    n = fromMaybe 3 $ readMaybe . unpack =<< listToMaybe nums


-- | @onKarmaFile action@ will parse the contents of the karma file and pass
-- the resulting 'Object' to @action@. If @action@ returns a new one, the
-- file will be replaced with that. If parsing the file fails, @action@ won't
-- be called but an error message will be sent to IRC and Syslog.
onKarmaFile :: (Object -> EventEnv (Maybe Object)) -> EventEnv ()
onKarmaFile action = do
    fileVar <- asks karmaFile
    file <- lift $ readMVar fileVar
    processFile <- asIO $ do
      eitherContent <- safeIO $ BL.readFile file
      case leftMap show eitherContent >>= eitherDecode of
        Left err -> do
          lift . syslog Error $ "onKarmaFile: " ++ err
          respond $ "Could not read karma file: " <> T.pack err
        Right obj -> do
          maybeObj' <- action obj
          for_ maybeObj' $ \obj' -> do
            writeResult <- safeIO $ do
              let newFile = file ++ ".new"
              BL.writeFile newFile (encode obj')
              renameFile newFile file
            case writeResult of
              Left err -> do
                lift . syslog Error $ "onKarmaFile: " ++ show err
                respond $ "Could not write karma file: " <> T.pack (show err)
              Right _ -> pure ()
    lift $ withMVar fileVar $ \_ -> processFile


toString :: [(Text, Integer)] -> Text
toString = T.intercalate ", " . map (\(nick, score) ->
    avoidHighlighting nick <> ": " <> pack (show score))


alarm :: [Text] -> EventEnv ()
alarm args = do
    alarm <- asks (alarmTopic . mqttEnv)
    publish alarm $ encodeUtf8 $ T.unwords args
    respond "ALAAAARM"

safeIO :: IO a -> EventEnv (Either IOError a)
safeIO = lift . tryIOError

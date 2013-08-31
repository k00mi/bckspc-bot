{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Monitor (monitor) where

import           Control.Monad
import           Control.Applicative
import           Control.Concurrent         (threadDelay)
import           Control.Exception          (catch, IOException)
import           Data.Monoid                ((<>))
import           Data.Char                  (isSpace)
import           Data.ByteString.Char8      (breakEnd, pack)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Network.SimpleIRC
import           Data.Aeson                 ((.:))
import qualified Data.Map                   as M
import           System.Posix.Syslog

import Config
import Utils


data Mode = None | Voice | Op deriving (Eq)


-- | Check the status API every 5 minutes to see if members are present.
-- Change channel topic and voice accordingly.
monitor :: Config -> MIrc -> IO ()
monitor cfg serv = forever $ do
    threadDelay $ 10^6 * 60 * 5 -- 5 minutes
    resp <- getJSON (statusUrl cfg) $ \obj ->
              (,) <$>  obj .: "members"
                  <*> (obj .: "members_present" >>= mapM (.: "nickname"))
    case resp of
      Left err -> syslog Warning $ "Error retrieving JSON: " ++ err
      Right (numMems, present) -> do
            changeTopic cfg serv numMems
            changeVoice cfg serv present
          `catch`
            \e -> syslog Warning $ "monitor: " ++ show (e :: IOException)


-- | Voice channel members who are currently present, devoice those who left.
changeVoice :: Config -> MIrc -> [Text] -> IO ()
changeVoice cfg serv present = do
    nicks <- getNicks (channel cfg) serv
    let (there, notThere) = M.partition (\(_, n) -> n `elem` present) nicks
        remove = M.filter ((== Voice) . fst) notThere
        give   = M.filter ((== None) . fst) there
        setMode mode nicks = flip M.traverseWithKey nicks $ \nick _ ->
          sendCmd serv $ MMode (pack $ channel cfg) mode $ Just $ encodeUtf8 nick
    setMode "+v" give
    setMode "-v" remove *> pure ()


-- | Set a new topic if open/close status changed.
changeTopic :: Config -> MIrc -> Int -> IO ()
changeTopic cfg serv num = do
    topic <- getNumericResponse serv "332" $ sendTopic Nothing
    let (static, current) = breakEnd isSpace topic
        new = if num == 0 then "closed" else "open"
    if current `notElem` ["closed", "open"]
      then syslog Warning "Topic not correctly formatted"
      else when (current /= new) . sendTopic . Just $ static <> new
  where
    sendTopic = sendCmd serv . MTopic (pack $ channel cfg)


-- | Get the members of the channel in a map from normalized nick to a pair
-- of voice status and actual name
getNicks :: String -> MIrc -> IO (M.Map Text (Mode,Text))
getNicks chan serv =
    M.fromList . map mkEntry . T.words . decodeUtf8 <$>
      getNumericResponse serv "353" cmd
  where
    cmd = sendRaw serv $ "NAMES " <> pack chan
    mkEntry rawNick =
        let mode = case T.head rawNick of
                       '@' -> Op;
                       '+' -> Voice;
                        _  -> None;
            nick = if mode /= None then T.tail rawNick else rawNick
        in (nick, (mode, sanitize nick))

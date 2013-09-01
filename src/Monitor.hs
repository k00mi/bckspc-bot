{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Monitor (monitor) where

import           Control.Monad
import           Control.Applicative
import           Control.Concurrent         (threadDelay)
import           Control.Exception          (catch, IOException)
import           Data.List                  (partition)
import           Data.Monoid                ((<>))
import           Data.Foldable              (for_)
import           Data.Char                  (isSpace)
import           Data.ByteString.Char8      (breakEnd, pack)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import           Network.SimpleIRC          hiding (Nick)
import           Data.Aeson                 ((.:))
import           System.Posix.Syslog

import Config
import Utils


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
    let prsnt = map sanitize present
    nicks <- getNicks (channel cfg) serv
    let (there, notThere) = partition (flip elem prsnt . sanitize . name) nicks
        remove = map name $ filter ((== Voice) . mode) notThere
        give   = map name $ filter ((== None) . mode) there
        setMode mode nicks = for_ nicks $ \nick ->
          sendCmd serv $ MMode (pack $ channel cfg) mode $ Just $ encodeUtf8 nick
    syslog Debug $ "Giving voice to: " ++ show give
    setMode "+v" give
    syslog Debug $ "Taking voice from: " ++ show remove
    setMode "-v" remove


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
getNicks :: String -> MIrc -> IO [Nick]
getNicks chan serv =
    map mkNick . T.words . T.tail . T.dropWhile (/= ':') . decodeUtf8 <$>
      getNumericResponse serv "353" cmd
  where
    cmd = sendRaw serv $ "NAMES " <> pack chan

data Nick = Nick
    { name :: Text
    , mode :: Mode
    }

data Mode = None | Voice | Op deriving (Eq)

mkNick :: Text -> Nick
mkNick rawNick = Nick (if mode /= None then T.tail rawNick else rawNick) mode
  where
    mode = case T.head rawNick of
             '@' -> Op
             '+' -> Voice
             _   -> None

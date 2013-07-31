{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Monitor (monitor) where

import           Control.Monad
import           Control.Applicative
import           Control.Concurrent         (threadDelay)
import           Data.Monoid                ((<>))
import           Data.Foldable              (for_)
import           Data.Char                  (isSpace)
import           Data.ByteString.Char8      (ByteString, breakEnd, pack)
import qualified Data.ByteString.Char8      as BS
import           Network.SimpleIRC
import           Data.Aeson                 ((.:))
import qualified Data.Map                   as M

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
    either
        (putStrLn . ("Error retrieving JSON: " ++))
        (\(numMems, present) -> do
            changeTopic cfg serv numMems
            changeVoice cfg serv present)
        resp


-- | Voice channel members who are currently present, devoice those who left.
changeVoice :: Config -> MIrc -> [ByteString] -> IO ()
changeVoice cfg serv present = do
    nicks <- getNicks (channel cfg) serv
    let (there, notThere) = M.partitionWithKey (\n _ -> n `elem` present) nicks
        remove = M.filter ((== Voice) . fst) notThere
        give   = M.filter ((== None) . fst) there
        setMode mode toSet = for_ toSet $ \(_, nick) ->
          sendCmd serv $ MMode (pack $ channel cfg) mode $ Just nick
    setMode "+v" give
    setMode "-v" remove


-- | Set a new topic if open/close status changed.
changeTopic :: Config -> MIrc -> Int -> IO ()
changeTopic cfg serv num = do
    topic <- getNumericResponse serv "332" $ sendTopic Nothing
    let (static, current) = breakEnd isSpace topic
        new = if num == 0 then "closed" else "open"
    if current `notElem` ["closed", "open"]
      then putStrLn "Topic not correctly formatted"
      else when (current /= new) . sendTopic . Just $ static <> new
  where
    sendTopic = sendCmd serv . MTopic (pack $ channel cfg)


-- | Get the members of the channel in a map from normalized nick to a pair
-- of voice status and actual name
getNicks :: String -> MIrc -> IO (M.Map ByteString (Mode,ByteString))
getNicks chan serv =
    M.fromList . map mkEntry . BS.words <$> getNumericResponse serv "353" cmd
  where
    cmd = sendRaw serv $ "NAMES " <> pack chan
    mkEntry rawNick =
        let mode = case BS.head rawNick of
                       '@' -> Op;
                       '+' -> Voice;
                        _  -> None;
            nick = if mode /= None then BS.tail rawNick else rawNick
        in (sanitize rawNick, (mode, nick))

{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Monitor (monitor) where

import           Control.Monad
import           Control.Applicative
import           Control.Concurrent.MVar    (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent         (threadDelay)
import           Data.Monoid                ((<>))
import           Data.Char                  (isSpace)
import           Data.ByteString.Char8      (ByteString, breakEnd, pack)
import qualified Data.ByteString.Char8      as BS
import           Network.SimpleIRC
import           Data.IORef                 (newIORef, readIORef, writeIORef)
import           Data.Aeson                 ((.:))
import qualified Data.Map                   as M

import Config
import Utils


-- | Check the status API every 5 minutes to see if members are present.
-- Change the channel topic accordingly.
monitor :: Config -> MIrc -> IO ()
monitor cfg serv = do
    topicVar <- newIORef "closed"
    forever $ do
      threadDelay $ 10^6 * 60 * 5 -- 5 minutes
      resp <- getJSON (statusUrl cfg) $ \obj ->
                (,) <$>  obj .: "members"
                    <*> (obj .: "members_present" >>= mapM (.: "nickname"))
      either
          (putStrLn . ("Error retrieving JSON: " ++))
          (\(numMems, present) -> do
              changeTopic cfg serv numMems topicVar
              changeVoice cfg serv present)
          resp


-- | Voice channel members who are currently present, devoice those who left.
changeVoice :: Config -> MIrc -> [ByteString] -> IO ()
changeVoice cfg serv present = do
    nicks <- getNicks (channel cfg) serv
    let (there, notThere) = M.partitionWithKey (\n _ -> n `elem` present) nicks
        remove = M.filter fst notThere
        give   = M.filter (not . fst) there

    let setMode mode toSet = unless (M.null toSet) $
            sendCmd serv $ MMode (pack $ channel cfg) mode $
                Just (BS.unwords $ map snd $ M.elems toSet)
    setMode "+v" give
    setMode "-v" remove

-- | Set a new topic if open/close status changed.
changeTopic :: Config -> MIrc -> Int -> IORef ByteString -> IO ()
changeTopic cfg serv n var = do
    let currentTop = if n == 0
                      then "closed"
                      else "open"
    oldTop <- readIORef var
    when (oldTop /= currentTop) $ do
        setTopic cfg serv currentTop
        writeIORef var currentTop


-- | Get the current topic by sending TOPIC, adding an event on the response
-- (332) and removing it immediately after receiving the message (via MVar).
-- Then change the last word of the topic (should be the status) to the new
-- value.
setTopic :: Config -> MIrc -> ByteString -> IO ()
setTopic cfg serv current = do
    topicFullVar <- newEmptyMVar
    eventID <- addEvent serv . Numeric $ \_ msg ->
                 when (mCode msg == "332") $ putMVar topicFullVar (mMsg msg)
    sendTopic Nothing
    (topicStatic, _) <- breakEnd isSpace <$> takeMVar topicFullVar
    remEvent serv eventID
    sendTopic . Just $ topicStatic <> current
  where
    sendTopic = sendCmd serv . MTopic (pack $ channel cfg)


-- | Get the members of the channel in a map from normalized nick to a pair
-- of voice status and actual name
getNicks :: String -> MIrc -> IO (M.Map ByteString (Bool,ByteString))
getNicks chan serv =
    M.fromList . map mkEntry . BS.words <$> getNumericResponse serv "353" cmd
  where
    cmd = sendRaw serv $ "NAMES " <> pack chan
    mkEntry rawNick =
        let voiced = BS.head rawNick `elem` "+@"
            nick = if voiced then BS.tail rawNick else rawNick
        in (sanitize rawNick, (voiced, nick))

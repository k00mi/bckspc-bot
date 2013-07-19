{-# LANGUAGE OverloadedStrings #-}

module Monitor (monitor) where

import           Control.Monad
import           Control.Applicative
import           Control.Concurrent.MVar    (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent         (threadDelay)
import           Data.Monoid                ((<>))
import           Data.Char                  (isSpace)
import           Data.ByteString.Char8      (ByteString, breakEnd, pack)
import           Network.SimpleIRC
import           Data.IORef                 (newIORef, readIORef, writeIORef)
import           Data.Aeson                 ((.:))

import Config
import Utils


-- | Check the status API every 5 minutes to see if members are present.
-- Change the channel topic accordingly.
monitor :: Config -> MIrc -> IO ()
monitor cfg serv = do
    topicVar <- newIORef "closed"
    forever $ do
      threadDelay $ 10^6 * 60 * 5 -- 5 minutes
      resp <- getJSON (statusUrl cfg) $ \obj -> obj .: "members"
      case resp of
          Left  _    -> pure ()
          Right mems -> do
              let currentTop = if mems == (0 :: Int)
                                 then "closed"
                                 else "open"
              oldTop <- readIORef topicVar
              when (oldTop /= currentTop) $ do
                  setTopic cfg serv currentTop
                  writeIORef topicVar currentTop


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

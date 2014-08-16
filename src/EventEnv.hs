{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module EventEnv
  ( MsgEnv(..)
  , MQTTEnv(..)
  , EventEnv
  , runEnv
  , lift
  , asIO
  , respond
  , respondNick
  , publish
  , ask
  , asks
  , runReaderT
  ) where

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class  (lift)
import           Control.Applicative
import           Control.Concurrent         (MVar)
import           Data.ByteString            (ByteString)
import           Data.Foldable              (for_)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (Text)
import           Data.Text.Encoding         (encodeUtf8, decodeUtf8)
import           MQTT                       (MQTT)
import qualified MQTT
import           Network.SimpleIRC


data MsgEnv = MsgEnv
            { server    :: MIrc
            , msg       :: IrcMessage
            , statusUrl :: String
            , karmaFile :: MVar String
            , mqttEnv   :: MQTTEnv
            }

data MQTTEnv = MQTTEnv
             { connection  :: Maybe MQTT
             , pizzaTopic  :: MQTT.Topic
             , alarmTopic  :: MQTT.Topic
             }


type EventEnv a = ReaderT MsgEnv IO a


runEnv :: EventEnv a -> String -> MVar String -> MIrc -> IrcMessage -> MQTTEnv
       -> IO a
runEnv env url file s message mqtt =
    runReaderT env $ MsgEnv s message url file mqtt


asIO :: EventEnv a -> EventEnv (IO a)
asIO env = do
    s <- asks server
    m <- asks msg
    url <- asks statusUrl
    file <- asks karmaFile
    mqtt <- asks mqttEnv
    return $ runEnv env url file s m mqtt


respond :: Text -> EventEnv ()
respond resp = do
    s <- asks server
    origin <- fromJust . mOrigin <$> asks msg
    lift $ sendMsg s origin $ encodeUtf8 resp


respondNick :: Text -> EventEnv ()
respondNick resp = do
    m <- asks msg
    let nick   = fromJust $ mNick m
        origin = fromJust $ mOrigin m
    respond $ if nick == origin
                then resp
                else decodeUtf8 nick <> ": " <> resp


publish :: MQTT.Topic -> ByteString -> EventEnv ()
publish topic payload = do
    mMqtt <- asks (connection . mqttEnv)
    for_ mMqtt $ \mqtt ->
      lift $ MQTT.publish mqtt MQTT.NoConfirm False topic payload

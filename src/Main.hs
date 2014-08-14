{-# LANGUAGE OverloadedStrings, TupleSections #-}

import           Control.Applicative
import           Control.Concurrent         (MVar, newMVar)
import           Control.Exception          (catch, SomeException(..))
import           Data.Foldable              (for_)
import           Data.Text                  (isPrefixOf)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import qualified Data.Map                   as M
import           Data.String
import           System.Environment         (lookupEnv)
import           MQTT                       (MQTT)
import qualified MQTT
import           MQTT.Logger
import           Network.SimpleIRC
import           System.Posix.Daemonize
import           System.Posix.Syslog

import EventEnv (runEnv, MQTTEnv(MQTTEnv))
import Commands
import Config
import Monitor
import Redmine


main :: IO ()
main = do
    mPath <- lookupEnv "BOT_CONFIG"
    eCfg  <- maybe
              (fatalError "BOT_CONFIG environment variable not set")
              readConfigFile
              mPath
    cfg   <- either
              (fatalError . ("Error reading config file: " ++))
              pure
              eCfg
    mqtt <- mqttConnect cfg
    serviced $ bot cfg mqtt

bot :: Config -> MQTT -> CreateDaemon ()
bot cfg mqtt = simpleDaemon
            { program          = const $ startBot cfg mqtt
            , user             = Just "ircbot"
            , pidfileDirectory = pidDir cfg
            }

startBot :: Config -> MQTT -> IO ()
startBot cfg mqtt = do
    fileVar <- newMVar (karmaFile cfg)
    let mqttEnv = MQTTEnv mqtt (fromString $ pizzaTopic cfg)
                               (fromString $ alarmTopic cfg)
    let ircCfg = (mkDefaultConfig (serv cfg) $ nick cfg)
                   { cChannels = [channel cfg]
                   , cPort     = port cfg
                   , cEvents   = [Privmsg $
                                    onMessage commands (statusUrl cfg) fileVar mqttEnv]
                   , cUsername = nick cfg
                   , cRealname = "bckspc"
                   , cPass     = password cfg
                   }
    res <- connect
            ircCfg
            True
            False
    either
      ioError
      (\s -> do
        initRedmine cfg s fileVar
        monitor cfg s)
      res


onMessage :: CommandMap -> String -> MVar String -> MQTTEnv -> EventFunc
onMessage cmds url fileVar mqtt s message =
    case T.words $ decodeUtf8 $ mMsg message of
        (name:"+1":_) -> applyCmd addKarma name
        (cmd:args)     | "!" `isPrefixOf` cmd
                      -> for_ (M.lookup (T.tail cmd) cmds) (`applyCmd` args)
        _             -> pure ()
  where
    applyCmd c args = runEnv (c args) url fileVar s message mqtt


mqttConnect :: Config -> IO MQTT
mqttConnect cfg = do
    mMqtt <- MQTT.connect MQTT.def
                   { MQTT.cHost = mqttHost cfg
                   , MQTT.cKeepAlive = Just 60
                   , MQTT.cClientID = "bckspc-bot"
                   , MQTT.cConnectTimeout = Just 10
                   , MQTT.cReconnPeriod = Just 10
                   , MQTT.cLogger = syslogLogger
                   }
    maybe (error "Server rufused connection") return mMqtt
  `catch`
    \(SomeException e) ->
        fatalError $ "Failed connecting to MQTT broker: " ++ show e
  where
    syslogLogger = Logger (syslog Info) (syslog Warning) (syslog Error)

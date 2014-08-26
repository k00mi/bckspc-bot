{-# LANGUAGE OverloadedStrings, TupleSections #-}

import           Control.Applicative
import           Control.Concurrent         (MVar, newMVar, threadDelay)
import           Control.Exception          (catch, SomeException(..))
import           Data.Foldable              (for_)
import           Data.Text                  (isPrefixOf)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import qualified Data.Map                   as M
import           Data.String
import           System.Environment         (lookupEnv)
import qualified Network.MQTT               as MQTT
import           Network.MQTT.Logger
import           Network.SimpleIRC
import           System.Posix.Daemonize
import           System.Posix.Syslog

import EventEnv (runEnv, MQTTEnv(MQTTEnv, connection))
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
    serviced $ bot cfg

bot :: Config -> CreateDaemon ()
bot cfg = simpleDaemon
            { program          = const $ startBot cfg
            , user             = Just "ircbot"
            , pidfileDirectory = pidDir cfg
            }

startBot :: Config -> IO ()
startBot cfg = do
    fileVar <- newMVar (karmaFile cfg)
    mqttEnv <- mqttConnect cfg
    let ircCfg = (mkDefaultConfig (serv cfg) $ nick cfg)
                   { cChannels = [channel cfg]
                   , cPort     = port cfg
                   , cEvents   = [ Privmsg $
                                    onMessage commands (statusUrl cfg) fileVar mqttEnv
                                 , Disconnect onDisconnect
                                 ]
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


mqttConnect :: Config -> IO MQTTEnv
mqttConnect cfg = do
    mMqtt <- MQTT.connect MQTT.defaultConfig
                   { MQTT.cHost = mqttHost cfg
                   , MQTT.cKeepAlive = Just 60
                   , MQTT.cClientID = "bckspc-bot"
                   , MQTT.cConnectTimeout = Just 10
                   , MQTT.cReconnPeriod = Just 10
                   , MQTT.cLogger = syslogLogger
                   }
    case mMqtt of
      Nothing -> errNoMqtt "Broker refused connection."
      mqtt    -> return $ defaultEnv { connection = mqtt }
  `catch`
    \(SomeException e) ->
        errNoMqtt $ "Failed connecting to server: " ++ show e
  where
    syslogLogger = Logger (syslog Info) (syslog Warning) (syslog Error)
    errNoMqtt err = defaultEnv <$ syslog Error ("No MQTT: " ++ err)
    defaultEnv = MQTTEnv Nothing (fromString $ pizzaTopic cfg)
                                 (fromString $ alarmTopic cfg)


onDisconnect :: MIrc -> IO ()
onDisconnect mirc = do
    syslog Error "Disconnected from IRC"
    go
  where
    go = do
      syslog Info "Reconnecting..."
      rslt <- reconnect mirc
      case rslt of
        Right _ -> syslog Info "Reconnect succesfull."
        Left err -> do
          syslog Error ("Reconnect failed: " ++ show err)
          threadDelay $ 10 * 10^6
          go

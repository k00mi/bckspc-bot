{-# LANGUAGE OverloadedStrings, TupleSections #-}

import           Control.Applicative
import           Control.Concurrent         (MVar, newMVar, threadDelay)
import           Control.Exception          (catch, SomeException(..))
import qualified Data.ByteString.Char8      as B
import           Data.Foldable              (for_)
import           Data.Text                  (isPrefixOf)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8With)
import           Data.Text.Encoding.Error   (lenientDecode)
import qualified Data.Map                   as M
import           Data.String
import qualified Network.MQTT               as MQTT
import           Network.SimpleIRC
import           System.Environment         (getArgs)
import           System.Exit                (exitFailure)
import           System.IO                  (hPutStrLn, stderr)

import EventEnv (runEnv, MQTTEnv(MQTTEnv, connection))
import Commands
import Config
import Monitor
import Redmine

defaultConfigPath :: String
defaultConfigPath = "/etc/bckspc-bot.conf"

usage :: IO a
usage = do
  hPutStrLn stderr "Usage: bckspc-bot [config]"
  exitFailure

main :: IO ()
main = do
    args <- getArgs
    cfg <- case args of
         [] -> readConfigFile defaultConfigPath
         [cfgPath] -> readConfigFile cfgPath
         _ -> usage
    startBot cfg

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
            (hPutStrLn stderr . B.unpack)
    either
      ioError
      (\s -> do
        initRedmine cfg s fileVar
        monitor cfg s)
      res


onMessage :: CommandMap -> String -> MVar String -> MQTTEnv -> EventFunc
onMessage cmds url fileVar mqtt s message =
    case T.words $ decodeUtf8With lenientDecode $ mMsg message of
        (name:"+1":_) -> applyCmd addKarma name
        (cmd:args)     | "!" `isPrefixOf` cmd
                      -> for_ (M.lookup (T.tail cmd) cmds) (`applyCmd` args)
        _             -> pure ()
  where
    applyCmd c args = runEnv (c args) url fileVar s message mqtt


mqttConnect :: Config -> IO MQTTEnv
mqttConnect cfg = do
    mqtt <- MQTT.connect MQTT.defaultConfig
                   { MQTT.cHost = mqttHost cfg
                   , MQTT.cKeepAlive = Just 60
                   , MQTT.cClientID = "bckspc-bot"
                   , MQTT.cConnectTimeout = Just 10
                   , MQTT.cReconnPeriod = Just 10
                   }
    return $ defaultEnv { connection = Just mqtt }
  `catch`
    \(SomeException e) ->
        errNoMqtt $ "Failed connecting to server: " ++ show e
  where
    errNoMqtt err = defaultEnv <$ hPutStrLn stderr ("No MQTT: " ++ err)
    defaultEnv = MQTTEnv Nothing (fromString $ pizzaTopic cfg)
                                 (fromString $ alarmTopic cfg)
                                 (fromString $ soundTopic cfg)


onDisconnect :: MIrc -> IO ()
onDisconnect mirc = do
    hPutStrLn stderr "Disconnected from IRC"
    go
  where
    go = do
      putStrLn "Reconnecting..."
      rslt <- reconnect mirc
      case rslt of
        Right _ -> putStrLn "Reconnect succesfull."
        Left err -> do
          hPutStrLn stderr ("Reconnect failed: " ++ show err)
          threadDelay $ 10 * 10^6
          go

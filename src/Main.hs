{-# LANGUAGE OverloadedStrings, TupleSections #-}

import           Control.Applicative
import           Control.Concurrent         (MVar, newMVar, threadDelay, forkIO)
import           Control.Concurrent.STM     (newTChanIO)
import           Control.Exception          (try, IOException)
import           Control.Monad              (forever)
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
    mqttEnv <- mkMqttEnv cfg
    forkIO $ forever $ do runMQTT (connection mqttEnv); threadDelay $ 30 * 10^6
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


mkMqttEnv :: Config -> IO MQTTEnv
mkMqttEnv cfg = do
    mqtt <- MQTT.defaultConfig <$> MQTT.mkCommands <*> newTChanIO
    return $ MQTTEnv
              mqtt { MQTT.cHost = mqttHost cfg
                   , MQTT.cKeepAlive = Just 60
                   , MQTT.cClientID = "bckspc-bot"
                   }
              (fromString $ pizzaTopic cfg)
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

runMQTT :: MQTT.Config -> IO ()
runMQTT mqtt = do
    putStrLn "[MQTT] Connecting"
    rslt <- try (MQTT.run mqtt)
    hPutStrLn stderr $
      "[MQTT] " ++ either (show :: IOException -> String) show rslt

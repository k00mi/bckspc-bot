module Config
  ( Config(..)
  , Redmine(..)
  , readConfigFile
  ) where

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString.Lazy       as BS
import           Data.Traversable           (traverse)
import           System.Exit                (exitFailure)
import           System.IO                  (hPutStrLn, stderr)


data Config = Config
            { statusUrl   :: String
            , karmaFile   :: FilePath
            , channel     :: String
            , pidDir      :: Maybe FilePath
            , serv        :: String
            , port        :: Int
            , nick        :: String
            , password    :: Maybe String
            , redmine     :: Maybe Redmine
            , mqttHost    :: String
            , pizzaTopic  :: String
            , alarmTopic  :: String
            , soundTopic  :: String
            }

data Redmine = Redmine
             { rmURL      :: String
             , rmProject  :: String
             , rmInterval :: Int
             , rmUser     :: Maybe String
             , rmPassword :: Maybe String
             }

instance FromJSON Redmine where
    parseJSON (Object v) = Redmine
                           <$> v .:  "url"
                           <*> v .:  "project"
                           <*> v .:  "interval"
                           <*> v .:? "user"
                           <*> v .:? "password"
    parseJSON _          = empty

instance FromJSON Config where
    parseJSON (Object v) = Config
                           <$> v .:  "statusUrl"
                           <*> v .:  "karmaFile"
                           <*> v .:  "channel"
                           <*> v .:? "pidDir"
                           <*> v .:? "server" .!= "chat.freenode.net"
                           <*> v .:? "port" .!= 6667
                           <*> v .:? "nick" .!= "b4ckspace"
                           <*> v .:? "password"
                           <*> (v .:? "redmine" >>= traverse parseJSON)
                           <*> v .:  "mqttHost"
                           <*> v .:  "pizzaTopic"
                           <*> v .:  "alarmTopic"
                           <*> v .:  "soundTopic"
    parseJSON _          = empty

readConfigFile :: FilePath -> IO Config
readConfigFile path = do
    config <- BS.readFile path
    case eitherDecode config of
         Right cfg -> return cfg
         Left err -> do
           hPutStrLn stderr ("Error reading config file: " ++ err)
           exitFailure

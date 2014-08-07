module Config
  ( Config(..)
  , Redmine(..)
  , readConfigFile
  ) where

import           Data.Aeson
import           Data.Traversable           (traverse)
import           Control.Applicative
import qualified Data.ByteString.Lazy       as BS


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
            }

data Redmine = Redmine
             { rmURL      :: String
             , rmInterval :: Int
             , rmUser     :: Maybe String
             , rmPassword :: Maybe String
             }

instance FromJSON Redmine where
    parseJSON (Object v) = Redmine
                           <$> v .:  "url"
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
    parseJSON _          = empty

readConfigFile :: FilePath -> IO (Either String Config)
readConfigFile = fmap eitherDecode . BS.readFile

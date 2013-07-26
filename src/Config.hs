module Config
  ( Config(..)
  , readConfigFile
  ) where

import           Data.Aeson
import           Control.Applicative
import qualified Data.ByteString.Lazy       as BS


data Config = Config
            { statusUrl :: String
            , karmaFile :: FilePath
            , channel   :: String
            , pidDir    :: Maybe FilePath
            }

instance FromJSON Config where
    parseJSON (Object v) = Config
                           <$> v .:  "statusUrl"
                           <*> v .:  "karmaFile"
                           <*> v .:  "channel"
                           <*> v .:? "pidDir"
    parseJSON _          = empty

readConfigFile :: FilePath -> IO (Either String Config)
readConfigFile = fmap eitherDecode . BS.readFile

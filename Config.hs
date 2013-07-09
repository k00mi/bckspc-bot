{-# LANGUAGE TemplateHaskell #-}

module Config
  ( Config(..)
  , readConfigFile
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy       as BS


data Config = Config { statusUrl :: String, karmaFile :: FilePath }

$(deriveJSON id ''Config)


readConfigFile :: FilePath -> IO (Either String Config)
readConfigFile = fmap eitherDecode . BS.readFile

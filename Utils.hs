{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( sanitize
  ) where

import           Data.Char                  (toLower, isLetter)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS


sanitize :: ByteString -> ByteString
sanitize = BS.map toLower . fst . BS.breakEnd isLetter

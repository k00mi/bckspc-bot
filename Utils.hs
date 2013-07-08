{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( sanitize
  ) where

import           Data.Char                  (isLetter)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS


sanitize :: ByteString -> ByteString
sanitize = fst . BS.breakEnd isLetter

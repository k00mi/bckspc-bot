{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( space
  , wordsBS
  , unwordsBS
  ) where

import           Data.Word
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS


space :: Word8
space = 32


wordsBS :: ByteString -> [ByteString]
wordsBS = BS.split space


unwordsBS :: [ByteString] -> ByteString
unwordsBS = BS.intercalate (BS.singleton space)

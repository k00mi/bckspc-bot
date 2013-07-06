{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( space
  , wordsBS
  , unwordsBS
  , respond
  , respondNick
  ) where

import Data.Word
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Maybe  (fromJust)
import qualified Data.ByteString as BS
import Network.SimpleIRC

space :: Word8
space = 32

wordsBS :: ByteString -> [ByteString]
wordsBS = BS.split space

unwordsBS :: [ByteString] -> ByteString
unwordsBS = BS.intercalate (BS.singleton space)

respond :: MIrc -> IrcMessage -> ByteString -> IO ()
respond s initial = sendMsg s (fromJust $ mOrigin initial)

respondNick :: MIrc -> IrcMessage -> ByteString -> IO ()
respondNick s init resp =
    respond s init $
      if nick == origin
        then resp
        else nick <> ": " <> resp
  where
    nick   = fromJust $ mNick init
    origin = fromJust $ mOrigin init

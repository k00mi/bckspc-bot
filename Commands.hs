{-# LANGUAGE OverloadedStrings #-}

module Commands (commands) where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.ByteString (ByteString)
import Network.SimpleIRC

import Utils

type CommandMap = Map ByteString ([ByteString] -> EventFunc)

commands :: CommandMap
commands = M.fromList [("echo", echo)
                      ]

echo :: [ByteString] -> EventFunc
echo args s msg = sendMsg s (fromJust $ mOrigin msg) $ unwordsBS args

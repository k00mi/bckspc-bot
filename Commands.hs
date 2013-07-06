{-# LANGUAGE OverloadedStrings #-}

module Commands
  ( commands
  , CommandMap
  ) where

import           Control.Monad
import           Data.Monoid                ((<>))
import           Data.Maybe
import           Data.ByteString.Char8      (pack)
import           Data.ByteString            (ByteString, cons)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map                   as M
import           Network.SimpleIRC
import           Network.HTTP
import           Data.Aeson
import           Data.Aeson.Types           (parseMaybe)

import Utils


type CommandMap = M.Map ByteString ([ByteString] -> EventFunc)

commands :: CommandMap
commands = M.fromList [ ("echo", echo)
                      , ("inspace", inspace)
                      ]


echo :: [ByteString] -> EventFunc
echo args s msg = respond s msg $ unwordsBS args


inspace :: [ByteString] -> EventFunc
inspace args s msg = do
    resp <- simpleHTTP (getRequest url) >>= getResponseBody
    let obj    = decode $ LBS.pack resp
        nicks  = maybe "Error fetching nicknames" unwordsBS $
          obj >>= parseMaybe (mapM (.: "nickname") <=< (.: "members_present"))
        opener = maybe "Members present: "
                       (\x -> pack (show (x :: Int )) <> " members present: ")
                       (obj >>= parseMaybe (.: "members"))
    respondNick s msg $ opener <> nicks
  where
    url = "http://status.bckspc.de/status.php?response=json"

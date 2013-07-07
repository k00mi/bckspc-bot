{-# LANGUAGE OverloadedStrings #-}

module Commands
  ( commands
  , CommandMap
  ) where

import           Control.Monad
import           Control.Applicative
import           Data.Monoid                ((<>))
import           Data.Maybe
import           Text.Read                  (readMaybe)
import           Data.Char                  (isDigit)
import           Data.ByteString            (ByteString, cons)
import           Data.ByteString.Char8      (pack, unpack)
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map                   as M
import           Control.Concurrent         (forkIO, threadDelay)
import           Network.SimpleIRC
import           Network.HTTP
import           Data.Aeson
import           Data.Aeson.Types           (parseMaybe)

import Utils


type CommandMap = M.Map ByteString ([ByteString] -> EventFunc)

commands :: CommandMap
commands = M.fromList [ ("echo", echo)
                      , ("inspace", inspace)
                      , ("pizza", pizza)
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


pizza :: [ByteString] -> EventFunc
pizza args s msg =
    if null args
      then notifyIn $ mins 15
      else maybe error notifyIn . getTime $ head args
  where
    notifyIn t = do
        forkIO $ do
            threadDelay t
            say "Time is up!"
        say "I won't forget it!"

    error = say "Could not parse duration"

    getTime str = do
        (numStr, suffix) <- BSC.unsnoc str
        if isDigit suffix
          then fmap mins . readMaybe $ unpack str
          else case suffix of
                's' -> secs  <$> readMaybe (unpack numStr);
                'm' -> mins  <$> readMaybe (unpack numStr);
                'h' -> hours <$> readMaybe (unpack numStr);
                 _  -> Nothing

    secs  x = x * 10^6
    mins  x = x * secs 60
    hours x = x * mins 60

    say = respondNick s msg

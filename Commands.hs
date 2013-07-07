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
import EventEnv


type CommandMap = M.Map ByteString ([ByteString] -> EventEnv ())

commands :: CommandMap
commands = M.fromList [ ("echo", echo)
                      , ("inspace", inspace)
                      , ("pizza", pizza)
                      ]


echo :: [ByteString] -> EventEnv ()
echo = respond . unwordsBS


inspace :: [ByteString] -> EventEnv ()
inspace args = do
    resp <- lift $ simpleHTTP (getRequest url) >>= getResponseBody
    let obj    = decode $ LBS.pack resp
        nicks  = maybe "Error fetching nicknames" unwordsBS $
          obj >>= parseMaybe (mapM (.: "nickname") <=< (.: "members_present"))
        opener = maybe "Members present: "
                       (\x -> pack (show (x :: Int )) <> " members present: ")
                       (obj >>= parseMaybe (.: "members"))
    respondNick $ opener <> nicks
  where
    url = "http://status.bckspc.de/status.php?response=json"


pizza :: [ByteString] -> EventEnv ()
pizza args =
    if null args
      then notifyIn $ mins 15
      else maybe error notifyIn . getTime $ head args
  where
    notifyIn t = do
        s <- asks server
        m <- asks msg
        lift . forkIO $ do
            threadDelay t
            runEnv (respondNick "Time is up!") s m
        respondNick "I won't forget it!"

    error = respondNick "Could not parse duration"

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

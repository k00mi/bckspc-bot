{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module EventEnv
  ( MsgEnv(..)
  , EventEnv
  , runEnv
  , lift
  , respond
  , respondNick
  , ask
  , asks
  , runReaderT
  ) where

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class  (lift)
import           Control.Applicative
import           Data.Maybe
import           Data.Monoid
import           Data.ByteString            (ByteString)
import           Network.SimpleIRC


data MsgEnv = MsgEnv
            { server    :: MIrc
            , msg       :: IrcMessage
            , statusUrl :: String
            , karmaFile :: String
            }


type EventEnv a = ReaderT MsgEnv IO a


runEnv :: EventEnv () -> String -> String -> EventFunc
runEnv env url file s message = runReaderT env $ MsgEnv s message url file


respond :: ByteString -> EventEnv ()
respond resp = do
    s <- asks server
    origin <- fromJust . mOrigin <$> asks msg
    lift $ sendMsg s origin resp


respondNick :: ByteString -> EventEnv ()
respondNick resp = do
    m <- asks msg
    let nick   = fromJust $ mNick m
        origin = fromJust $ mOrigin m
    respond $ if nick == origin
                then resp
                else nick <> ": " <> resp

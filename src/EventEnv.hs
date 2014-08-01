{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module EventEnv
  ( MsgEnv(..)
  , EventEnv
  , runEnv
  , lift
  , asIO
  , respond
  , respondNick
  , ask
  , asks
  , runReaderT
  ) where

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class  (lift)
import           Control.Applicative
import           Control.Concurrent         (MVar, withMVar)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (Text)
import           Data.Text.Encoding         (encodeUtf8, decodeUtf8)
import           Network.SimpleIRC


data MsgEnv = MsgEnv
            { server    :: MIrc
            , msg       :: IrcMessage
            , statusUrl :: String
            , karmaFile :: MVar String
            }


type EventEnv a = ReaderT MsgEnv IO a


runEnv :: EventEnv a -> String -> MVar String -> MIrc -> IrcMessage -> IO a
runEnv env url file s message = runReaderT env $ MsgEnv s message url file


asIO :: EventEnv a -> EventEnv (IO a)
asIO env = do
    s <- asks server
    m <- asks msg
    url <- asks statusUrl
    file <- asks karmaFile
    return $ runEnv env url file s m


respond :: Text -> EventEnv ()
respond resp = do
    s <- asks server
    origin <- fromJust . mOrigin <$> asks msg
    lift $ sendMsg s origin $ encodeUtf8 resp


respondNick :: Text -> EventEnv ()
respondNick resp = do
    m <- asks msg
    let nick   = fromJust $ mNick m
        origin = fromJust $ mOrigin m
    respond $ if nick == origin
                then resp
                else decodeUtf8 nick <> ": " <> resp

{-# LANGUAGE OverloadedStrings, TupleSections #-}

import           Control.Monad
import           Control.Applicative
import           Data.Maybe
import           Data.ByteString            (ByteString, isPrefixOf)
import qualified Data.ByteString            as BS
import qualified Data.Map                   as M
import           Network.SimpleIRC

import EventEnv
import Commands
import Utils


cfg = mkDefaultConfig "chat.freenode.net" "koomis-bot"

main :: IO ()
main = do
    connect cfg { cChannels = ["#koomis-bot"]
                , cEvents = [Privmsg $ onMessage commands]
                }
            False
            True
    putStrLn "exiting..."


onMessage :: CommandMap -> EventFunc
onMessage cmds s msg = -- do
    when ("!" `isPrefixOf` m) $ do
      let maybeResp = do
            (cmd, args) <- parseCmd m
            f <- M.lookup cmd cmds
            return $ runEnv (f args) s msg
      fromMaybe (pure ()) maybeResp
  where
    chan = fromJust $ mChan msg
    nick = fromJust $ mNick msg
    orig = fromJust $ mOrigin msg
    m    = mMsg msg


parseCmd :: ByteString -> Maybe (ByteString, [ByteString])
parseCmd msg = (, args) <$> listToMaybe cmds
  where
    (cmds, args) = splitAt 1 . wordsBS $ BS.tail msg

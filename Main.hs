{-# LANGUAGE OverloadedStrings, TupleSections #-}

import Control.Monad
import Control.Applicative
import Network.SimpleIRC
import qualified Data.Map as M
import Data.ByteString (ByteString, isPrefixOf)
import qualified Data.ByteString as BS
import Data.Maybe

import Commands
import Utils

cfg = mkDefaultConfig "chat.freenode.net" "koomis-bot"

main :: IO ()
main = do
    connect cfg { cChannels = ["#koomis-bot"], cEvents = [Privmsg onMessage] }
            False
            True
    putStrLn "exiting..."


onMessage :: EventFunc
onMessage s msg = -- do
    when ("!" `isPrefixOf` m) $ do
      let mCmd = do
            (cmd, args) <- parseCmd m
            f <- M.lookup cmd commands
            return $ f args s msg
      fromMaybe (pure ()) mCmd
  where
    chan = fromJust $ mChan msg
    nick = fromJust $ mNick msg
    orig = fromJust $ mOrigin msg
    m    = mMsg msg


parseCmd :: ByteString -> Maybe (ByteString, [ByteString])
parseCmd msg = (, args) <$> listToMaybe cmds
  where
    (cmds, args) = splitAt 1 . wordsBS $ BS.tail msg

{-# LANGUAGE OverloadedStrings, TupleSections #-}

import           Control.Monad
import           Control.Applicative
import           Data.Maybe
import           Data.ByteString            (ByteString, isPrefixOf)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC
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
onMessage cmds s msg =
    case BSC.words (mMsg msg) of
        (cmd:args)     | "!" `isPrefixOf` cmd
                      -> maybe (pure ()) (`applyCmd` args) $
                           M.lookup (BS.tail cmd) cmds
        _             -> pure ()
  where
    applyCmd c args = runEnv (c args) s msg

{-# LANGUAGE OverloadedStrings, TupleSections #-}

import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Data.ByteString            (isPrefixOf)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC
import qualified Data.Map                   as M
import           Control.Concurrent         (forkIO)
import           System.Environment         (lookupEnv)
import           Network.SimpleIRC
import           System.Posix.Daemonize

import EventEnv
import Commands
import Utils
import Config
import Monitor


ircCfg = mkDefaultConfig "chat.freenode.net" "bckspc-bot"

main :: IO ()
main = do
    mPath <- lookupEnv "BOT_CONFIG"
    eCfg  <- maybe
              (fatalError "BOT_CONFIG environment variable not set")
              readConfigFile
              mPath
    cfg   <- either
              (fatalError . ("Error reading config file: " ++))
              pure
              eCfg
    serviced $ bot cfg

bot :: Config -> CreateDaemon ()
bot cfg = simpleDaemon
            { program          = const $ startBot cfg
            , user             = Just "ircbot"
            , pidfileDirectory = pidDir cfg
            }

startBot :: Config -> IO ()
startBot cfg = do
    res <- connect
            ircCfg { cChannels = [channel cfg]
                   , cEvents   = [Privmsg $ onMessage commands cfg]
                   , cUsername = "bckspc"
                   , cRealname = "bckspc"
                   }
            True
            False
    either
      ioError
      (void . monitor cfg)
      res


onMessage :: CommandMap -> Config -> EventFunc
onMessage cmds (Config url file chan _) s msg =
    case BSC.words (mMsg msg) of
        (name:"+1":_) -> applyCmd addKarma $ sanitize name
        (cmd:args)     | "!" `isPrefixOf` cmd
                      -> maybe (pure ()) (`applyCmd` args) $
                           M.lookup (BS.tail cmd) cmds
        _             -> pure ()
  where
    applyCmd c args = runEnv (c args) url file s msg

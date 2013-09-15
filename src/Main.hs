{-# LANGUAGE OverloadedStrings, TupleSections #-}

import           Control.Applicative
import           Control.Monad
import           Data.Foldable              (for_)
import           Data.Text                  (isPrefixOf)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import qualified Data.Map                   as M
import           System.Environment         (lookupEnv)
import           Network.SimpleIRC
import           System.Posix.Daemonize

import EventEnv (runEnv)
import Commands
import Config
import Monitor


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
    let ircCfg = (mkDefaultConfig (serv cfg) $ nick cfg)
                   { cChannels = [channel cfg]
                   , cPort     = port cfg
                   , cEvents   = [Privmsg $ onMessage commands cfg]
                   , cUsername = nick cfg
                   , cRealname = "bckspc"
                   , cPass     = password cfg
                   }
    res <- connect
            ircCfg
            True
            False
    either
      ioError
      (void . monitor cfg)
      res


onMessage :: CommandMap -> Config -> EventFunc
onMessage cmds (Config{statusUrl = url, karmaFile = file}) s message =
    case T.words $ decodeUtf8 $ mMsg message of
        (name:"+1":_) -> applyCmd addKarma name
        (cmd:args)     | "!" `isPrefixOf` cmd
                      -> for_ (M.lookup (T.tail cmd) cmds) (`applyCmd` args)
        _             -> pure ()
  where
    applyCmd c args = runEnv (c args) url file s message

{-# LANGUAGE OverloadedStrings, TupleSections #-}

import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Data.ByteString            (isPrefixOf)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC
import qualified Data.Map                   as M
import           System.Environment         (getArgs)
import           System.Exit                (exitFailure)
import           Network.SimpleIRC

import EventEnv
import Commands
import Utils
import Config
import Monitor


ircCfg :: IrcConfig
ircCfg = mkDefaultConfig "chat.freenode.net" "bckspc-bot"

main :: IO ()
main = do
    mArg <- listToMaybe <$> getArgs
    userCfg <- maybe
                (putStrLn "Missing argument: path to config" >> exitFailure)
                readConfigFile
                mArg
    cfg <- case userCfg of
             Left err -> do
               putStr "Error reading config file: "
               putStrLn err
               exitFailure
             Right cfg -> pure cfg
    res <- connect
            ircCfg { cChannels = [channel cfg]
                   , cEvents   = [Privmsg $ onMessage commands cfg]
                   , cUsername = "bckspc"
                   , cRealname = "bckspc"
                   }
            True
            True
    either
      ioError
      (void . monitor cfg)
      res


onMessage :: CommandMap -> Config -> EventFunc
onMessage cmds (Config url file _) s message =
    case BSC.words (mMsg message) of
        (name:"+1":_) -> applyCmd addKarma $ sanitize name
        (cmd:args)     | "!" `isPrefixOf` cmd
                      -> maybe (pure ()) (`applyCmd` args) $
                           M.lookup (BS.tail cmd) cmds
        _             -> pure ()
  where
    applyCmd c args = runEnv (c args) url file s message

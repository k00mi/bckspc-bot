{-# LANGUAGE OverloadedStrings, TupleSections, MultiWayIf #-}

module Commands
  ( commands
  , CommandMap
  , addKarma
  ) where

import           Control.Monad
import           Control.Applicative
import           Data.Monoid                ((<>))
import           Data.Maybe
import           Data.Foldable              (for_)
import           Data.List                  (sortBy)
import           Data.Ord                   (comparing, Down(..))
import           Text.Read                  (readMaybe)
import qualified Data.ByteString.Lazy       as BL
import           Data.Text                  (Text, pack, unpack)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import           Data.Text.Read             (decimal)
import qualified Data.Map                   as M
import qualified Data.HashMap.Strict        as HM
import           Control.Concurrent         (forkIO, threadDelay)
import           System.IO.Error            (tryIOError)
import           System.IO
import           Network.SimpleIRC
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Attoparsec.Number     (Number(..))
import           System.Posix.Syslog

import EventEnv
import Utils


type CommandMap = M.Map Text ([Text] -> EventEnv ())

commands :: CommandMap
commands = M.fromList [ ("echo", echo)
                      , ("inspace", inspace)
                      , ("pizza", pizza)
                      , ("karma", karma)
                      , ("karmatop", karmatop)
                      , ("alarm", alarm)
                      , ("i", inspace)
                      , ("p", pizza)
                      , ("a", alarm)
                      , ("k", karma)
                      ]


echo :: [Text] -> EventEnv ()
echo = respond . T.unwords


inspace :: [Text] -> EventEnv ()
inspace _ = do
    url <- asks statusUrl
    res <- lift . getJSON url $ \obj ->
             (,) <$>  obj .: "members"
                 <*> (obj .: "members_present" >>= mapM (.: "nickname"))
    response <- case res of
            Left err -> do
              lift . syslog Warning $ "inspace: Error fetching JSON: " ++ err
              pure $ "Error retrieving status information"
            Right (num, nicks)
                | num == (0 :: Int) -> pure $ "Backspace is empty"
                | otherwise         -> pure $ pack (show num)
                                            <> " members present: "
                                            <> T.intercalate ", " nicks
    respondNick response


pizza :: [Text] -> EventEnv ()
pizza args =
    case args of
      []    -> notifyIn $ mins 15
      arg:_ -> either (const parseErr) notifyIn $ getTime arg
  where
    notifyIn t = do
        s <- asks server
        m <- asks msg
        url <- asks statusUrl
        file <- asks karmaFile
        lift . forkIO $ do
            threadDelay t
            runEnv (respondNick "Time is up!") url file s m
        respondNick "I won't forget it!"

    parseErr = respondNick "Could not parse duration"

    getTime str = do
        (num, rest) <- decimal str
        case rest of
          "s" -> Right $ secs num
          "m" -> Right $ mins num
          "h" -> Right $ hours num
          ""  -> Right $ mins num
          _   -> Left "invalid suffix"

    secs  x = x * 10^6
    mins  x = x * secs 60
    hours x = x * mins 60


addKarma :: Text -> EventEnv ()
addKarma nick = do
    message <- asks msg
    let sender = sanitize . decodeUtf8 . fromJust . mNick $ message
        nick'  = sanitize nick
    if | isPM message -> respondNick "You can only give karma in the channel"
       | nick' == sender -> respondNick "You can't give yourself karma"
       | otherwise -> onKarmaFile $
            pure . Just . HM.insertWith add nick' (Number 1)
  where
    add (Number x) (Number y) = Number $ x + y
    add _ _ = error "add: Adding non-Numbers"


karma :: [Text] -> EventEnv ()
karma nicks = onKarmaFile $ \obj -> do
    sender <- asks $ fromJust . mNick . msg
    let resp = flip parseMaybe obj $ \o ->
          if not $ null nicks
            then fmap toString $
                 forM nicks $ \n ->
                    let nick = sanitize n
                    in (nick,) <$> o .:? nick .!= 0
            else do
              score <- o .:? decodeUtf8 sender :: Parser (Maybe Integer)
              return $ maybe
                "You have no karma yet."
                (\n -> "You have " <> pack (show n) <> " karma.")
                score
    maybe
      (respondNick "Error parsing karma file")
      respondNick
      resp
    pure Nothing


karmatop :: [Text] -> EventEnv ()
karmatop nums = onKarmaFile $
      (pure Nothing <*)
      . respondNick
      . toString
      . take n
      . sortBy (comparing $ Down . snd)
      . map (\(name, Number (I x)) -> (name, x))
      . HM.toList
  where
    n = fromMaybe 3 $ readMaybe . unpack =<< listToMaybe nums


-- | @onKarmaFile action@ will parse the contents of the karma file and pass
-- the resulting 'Object' to @action@. If @action@ returns a new one, the
-- file will be replaced with that. If parsing the file fails, @action@ won't
-- be called but an error message will be sent to IRC and Syslog.
onKarmaFile :: (Object -> EventEnv (Maybe Object)) -> EventEnv ()
onKarmaFile action = do
    file   <- asks karmaFile
    msgenv <- ask
    res <- lift . tryIOError . withFile file ReadWriteMode $ \h -> do
        size    <- hFileSize h
        content <- BL.hGet h (fromInteger size)
        res     <- maybe
                    (fail "Failed reading karma file")
                    (\obj -> runReaderT (action obj) msgenv)
                    (decode content)
        for_ res $ (hSeek h AbsoluteSeek 0 >>) . BL.hPut h . encode
    either
      (\err -> do
        lift . syslog Warning $ "onKarmaFile: " ++ show err
        respond "Could not read karma file.")
      pure
      res


toString :: [(Text, Integer)] -> Text
toString = T.intercalate ", " . map (\(nick, score) ->
    nick <> ": " <> pack (show score))


alarm :: [Text] -> EventEnv ()
alarm = lift . broadcast "irc_alarm" . T.unwords

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
import           Data.List                  (sortBy)
import           Data.Ord                   (comparing, Down(..))
import           Text.Read                  (readMaybe)
import           Data.Char                  (isDigit)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (pack, unpack)
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy       as BL
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import qualified Data.Map                   as M
import qualified Data.HashMap.Strict        as HM
import           Control.Concurrent         (forkIO, threadDelay)
import           System.IO.Error            (tryIOError)
import           System.IO
import           Network.SimpleIRC
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Attoparsec.Number     (Number(..))

import EventEnv
import Utils


type CommandMap = M.Map ByteString ([ByteString] -> EventEnv ())

commands :: CommandMap
commands = M.fromList [ ("echo", echo)
                      , ("inspace", inspace)
                      , ("pizza", pizza)
                      , ("karma", karma)
                      , ("karmatop", karmatop)
                      , ("alarm", alarm)
                      ]


echo :: [ByteString] -> EventEnv ()
echo = respond . BSC.unwords


inspace :: [ByteString] -> EventEnv ()
inspace _ = do
    url <- asks statusUrl
    res <- lift . getJSON url $ \obj ->
             (,) <$>  obj .: "members"
                 <*> (obj .: "members_present" >>= mapM (.: "nickname"))
    let response =
          case res of
            Left err -> "Error retrieving JSON: " <> pack err
            Right (num, nicks)
                | num == (0 :: Int) -> "Backspace is empty"
                | otherwise         -> pack (show num)
                                       <> " members present: "
                                       <> BSC.unwords nicks
    respondNick response


pizza :: [ByteString] -> EventEnv ()
pizza args =
    if null args
      then notifyIn $ mins 15
      else maybe parseErr notifyIn . getTime $ head args
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


addKarma :: ByteString -> EventEnv ()
addKarma nick = do
    message <- asks msg
    sender  <- asks $ sanitize . fromJust . mNick . msg
    let nick' = sanitize nick
    if | isPM message -> respondNick "You can only give karma in the channel"
       | nick' == sender -> respondNick "You can't give yourself karma"
       | otherwise -> onKarmaFile $
            pure . Just . HM.insertWith add (decodeUtf8 nick') (Number 1)
  where
    add (Number x) (Number y) = Number $ x + y
    add _ _ = error "add: Adding non-Numbers"


karma :: [ByteString] -> EventEnv ()
karma nicks = onKarmaFile $ \obj -> do
    sender <- asks $ fromJust . mNick . msg
    let resp = flip parseMaybe obj $ \o ->
          if not $ null nicks
            then fmap toString $
                 forM nicks $ \n ->
                    let nick = decodeUtf8 $ sanitize n
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


karmatop :: [ByteString] -> EventEnv ()
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
-- be called but an error message will be sent to IRC.
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
        maybe
          (pure ())
          ((hSeek h AbsoluteSeek 0 >>) . BL.hPut h . encode)
          res
    either
      (respond . ("Error: " <>) . pack . show)
      pure
      res


toString :: [(Text, Integer)] -> ByteString
toString = BSC.intercalate ", " . map (\(nick, score) ->
    encodeUtf8 nick <> ": " <> pack (show score))


alarm :: [ByteString] -> EventEnv ()
alarm = lift . broadcast "irc_alarm" . BSC.unwords

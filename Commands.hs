{-# LANGUAGE OverloadedStrings #-}

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
import           Data.Char                  (isDigit, isSpace)
import           Data.ByteString            (ByteString, isPrefixOf)
import           Data.ByteString.Char8      (pack, unpack, snoc)
import qualified Data.ByteString.Char8      as BSC
import qualified Data.Map                   as M
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Exception          (try, SomeException)
import           Network.SimpleIRC
import           Data.Aeson

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
      else maybe error notifyIn . getTime $ head args
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

    error = respondNick "Could not parse duration"

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
addKarma nick = flip onKarmaFile pure $ \file -> do
    ls <- BSC.lines <$> BSC.readFile file
    let (new, present) = foldl checkAdd (BSC.empty, False) ls
    BSC.writeFile file $
        if present
          then new
          else new <> nick <> " 1\n"
  where
    checkAdd (file, True) bs = (file <> bs `snoc` '\n', True)
    checkAdd (file, _)    bs
      | nick `isPrefixOf` bs =
          let new :: Int
              new = (+1) . read . unpack $ BSC.dropWhile (not . isSpace) bs
          in (file <> nick <> pack (' ' : show new) `snoc` '\n', True)
      | otherwise            = (file <> bs `snoc` '\n', False)


karma :: [ByteString] -> EventEnv ()
karma nicks =
    onKarmaFile
      BSC.readFile
      ( respondNick <=< mkResponse
      . map (BSC.break isSpace)
      . BSC.lines
      )
  where
    mkResponse :: [(ByteString, ByteString)] -> EventEnv ByteString
    mkResponse entries =
      if null nicks
        then do
          n <- asks $ fromJust . mNick . msg
          let points = lookup (sanitize n) entries
          pure $ maybe
                   "You have no karma"
                   (\ps -> "You have" <> ps <> " karma points")
                      points
        else pure . prettify $ filter
                                  (flip elem (map sanitize nicks) . fst)
                                  entries


karmatop :: [ByteString] -> EventEnv ()
karmatop nums =
    onKarmaFile
      BSC.readFile
      ( respondNick
      . prettify
      . take n
      . sortBy (comparing $ Down . (read :: String -> Int) . unpack . snd)
      . map (BSC.break isSpace)
      . BSC.lines
      )
  where
    n = fromMaybe 3 $ readMaybe . unpack =<< listToMaybe nums


onKarmaFile :: (FilePath -> IO a) -> (a -> EventEnv ()) -> EventEnv ()
onKarmaFile io action =
    asks karmaFile >>= lift . try . io >>= either errorResponse action


prettify :: [(ByteString, ByteString)] -> ByteString
prettify = BSC.intercalate ", " . map go
  where
    go (nick, points) = (nick `BSC.snoc` ':') <> points


errorResponse :: SomeException -> EventEnv ()
errorResponse = respond . ("Error: " <>) . pack . show


alarm :: [ByteString] -> EventEnv ()
alarm = lift . broadcast "irc_alarm" . BSC.unwords

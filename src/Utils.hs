{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( sanitize
  , avoidHighlighting
  , increment
  , getJSONWith
  , getJSON
  , getMembersPresent
  , getURLTransformRQ
  , getURL
  , leftMap
  , getNumericResponse
  , isPM
  ) where

import           Control.Monad
import           Control.Applicative
import           Control.Exception          (catch)
import           Data.Maybe                 (fromMaybe)
import           Data.Char                  (toLower, isLetter)
import           Data.ByteString            (ByteString)
import           Data.Text                  (Text, dropAround)
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy       as LBS
import           Control.Concurrent.MVar
import           Network.SimpleIRC
import           Network.HTTP.Client        (parseUrl, newManager, httpLbs,
                                            HttpException, responseBody,
                                            Request)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Data.Aeson                 (eitherDecode, (.:), Value(..),
                                            Object)
import           Data.Aeson.Types           (parseEither, FromJSON, Parser)
import           Data.HashMap.Strict        (insertWith, (!))


sanitize :: Text -> Text
sanitize = T.map toLower . dropAround (not . isLetter)


avoidHighlighting :: Text -> Text
avoidHighlighting nick = maybe nick insertUnicode $ T.uncons nick
  where
    insertUnicode (c,cs) = c `T.cons` '\8288' `T.cons` cs


increment :: Text -> Object -> (Int, Object)
increment nick o = (truncate new, o')
  where
    o' = insertWith add nick (Number 1) o
    Number new = o' ! nick

    add (Number x) (Number y) = Number $ x + y
    add _ _ = error "add: Object contains non-Numbers."


getURLTransformRQ :: (Request -> Request) -> String -> IO LBS.ByteString
getURLTransformRQ f url = do
    req <- f <$> parseUrl url
    man <- newManager tlsManagerSettings
    responseBody <$> httpLbs req man


getURL :: String -> IO LBS.ByteString
getURL = getURLTransformRQ id


getJSONWith :: FromJSON a
            => IO LBS.ByteString -> (a -> Parser b) -> IO (Either String b)
getJSONWith get parser =
    fmap (parseEither parser <=< eitherDecode) get
  `catch`
    \e -> return $ Left $ show (e :: HttpException)

getJSON :: FromJSON a => String -> (a -> Parser b) -> IO (Either String b)
getJSON = getJSONWith . getURL


getMembersPresent :: String -> IO (Either String (Int, [Text]))
getMembersPresent url = getJSON url $ \obj -> do
    rooms <- obj .: "sensors" >>= (.: "people_now_present")
    let f (num, names) o = do
          numLocal <- o .: "value"
          namesLocal <- o .: "names"
          return (num + numLocal, namesLocal ++ names)
    foldM f (0, []) rooms


leftMap :: (a -> b) -> Either a c -> Either b c
leftMap f (Left x)  = Left $ f x
leftMap _ (Right x) = Right x


getNumericResponse :: MIrc -> ByteString -> IO () -> IO ByteString
getNumericResponse serv code command = do
    respVar <- newEmptyMVar
    eventID <- addEvent serv . Numeric $ \_ msg ->
                  when (mCode msg == code) $ putMVar respVar (mMsg msg)
    command
    takeMVar respVar <* remEvent serv eventID


isPM :: IrcMessage -> Bool
isPM msg = fromMaybe False $ do
    nick   <- mNick msg
    (nick ==) <$> mOrigin msg

{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( sanitize
  , avoidHighlighting
  , broadcast
  , getJSON
  , getMembersPresent
  , getURL
  , leftMap
  , getNumericResponse
  , isPM
  ) where

import           Control.Monad
import           Control.Applicative
import           Control.Exception          (catch)
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Char                  (toLower, isLetter)
import           Data.ByteString            (ByteString)
import           Data.Text                  (Text, dropAround)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.ByteString.Lazy       as LBS
import           Control.Concurrent.MVar
import           Network.SimpleIRC
import           Network.Socket             hiding (sendTo)
import           Network.Socket.ByteString  (sendTo)
import           Network.BSD                (getProtocolNumber)
import           Network.HTTP.Client        (parseUrl, newManager, httpLbs,
                                            HttpException, responseBody)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Data.Aeson                 (eitherDecode, (.:))
import           Data.Aeson.Types           (parseEither, FromJSON, Parser)


sanitize :: Text -> Text
sanitize = T.map toLower . dropAround (not . isLetter)


avoidHighlighting :: Text -> Text
avoidHighlighting nick = maybe nick insertUnicode $ T.uncons nick
  where
    insertUnicode (c,cs) = c `T.cons` '\8288' `T.cons` cs


broadcast :: Text -> Text -> IO ()
broadcast name msg = void $ do
    let addr = SockAddrInet 5042 0xffffffff
    proto <- getProtocolNumber "udp"
    sock <- socket AF_INET Datagram proto
    setSocketOption sock Broadcast sOL_SOCKET
    sendTo sock ("COMMON,0," <> encodeUtf8 (T.snoc name ',') <> encodeUtf8 msg) addr


getURL :: String -> IO LBS.ByteString
getURL url = do
    req <- parseUrl url
    man <- newManager tlsManagerSettings
    responseBody <$> httpLbs req man


getJSON :: FromJSON a => String -> (a -> Parser b) -> IO (Either String b)
getJSON url parser =
    fmap (parseEither parser <=< eitherDecode) (getURL url)
  `catch`
    \e -> return $ Left $ show (e :: HttpException)


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

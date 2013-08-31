{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( sanitize
  , broadcast
  , getJSON
  , getURL
  , leftMap
  , getNumericResponse
  , isPM
  ) where

import           Control.Monad
import           Control.Applicative
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                ((<>))
import           Data.Char                  (toLower, isLetter)
import           Data.ByteString            (ByteString)
import           Data.Text                  (Text, dropAround)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as LBS
import           Control.Concurrent.MVar
import           Network.SimpleIRC
import           Network.Socket             hiding (sendTo)
import           Network.Socket.ByteString  (sendTo)
import           Network.BSD                (getProtocolNumber)
import           Network.HTTP               (simpleHTTP, rspBody)
import           Network.HTTP.Base          (mkRequest, RequestMethod(GET))
import           Network.URI                (parseURI)
import           Data.Aeson                 (eitherDecode)
import           Data.Aeson.Types           (parseEither, FromJSON, Parser)


sanitize :: Text -> Text
sanitize = T.map toLower . dropAround (not . isLetter)


broadcast :: Text -> Text -> IO ()
broadcast name msg = void $ do
    let addr = SockAddrInet 5042 0xffffffff
    proto <- getProtocolNumber "udp"
    sock <- socket AF_INET Datagram proto
    setSocketOption sock Broadcast sOL_SOCKET
    sendTo sock ("COMMON,0," <> encodeUtf8 (T.snoc name ',') <> encodeUtf8 msg) addr


getURL :: String -> IO (Either String LBS.ByteString)
getURL url =
    maybe
      (return . Left $ "Invalid URI: " ++ url)
      (fmap (leftMap show . fmap rspBody) . simpleHTTP . mkRequest GET)
      (parseURI url)


getJSON :: FromJSON a => String -> (a -> Parser b) -> IO (Either String b)
getJSON url parser =
    leftMap ("getJSON: " ++) . ((parseEither parser <=< eitherDecode) =<<)
    <$> getURL url


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


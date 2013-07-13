{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( sanitize
  , broadcast
  ) where

import           Control.Monad
import           Data.Monoid
import           Data.Char                  (toLower, isLetter)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import           Network.Socket             hiding (sendTo)
import           Network.Socket.ByteString  (sendTo)
import           Network.BSD                (getProtocolNumber)


sanitize :: ByteString -> ByteString
sanitize = BS.map toLower . fst . BS.breakEnd isLetter


broadcast :: ByteString -> ByteString -> IO ()
broadcast name msg = void $ do
    let addr = SockAddrInet 5042 0xffffffff
    proto <- getProtocolNumber "udp"
    sock <- socket AF_INET Datagram proto
    setSocketOption sock Broadcast sOL_SOCKET
    sendTo sock ("COMMON,0," <> BS.snoc name ',' <> msg) addr

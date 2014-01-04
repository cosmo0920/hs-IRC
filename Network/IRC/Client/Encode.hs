module Network.IRC.Client.Encode
  ( packWithEncoding
  , unpackWithEncoding
  , fromStrict' ) where

import Data.Text.Encoding
import qualified Data.Text                     as T
import qualified Data.ByteString               as P (ByteString) -- type name only
import qualified Data.ByteString               as S
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import Prelude

-- | Convert String to ByteString(Strict) with Encoding.
packWithEncoding :: String -> B.ByteString
packWithEncoding = encodeUtf8 . T.pack

-- | Convert ByteString to String with Encoding
unpackWithEncoding :: B.ByteString -> String
unpackWithEncoding = T.unpack . decodeUtf8

-- |/O(1)/ Convert a strict 'ByteString' into a lazy 'ByteString'.
--
--  sadly hack...
--
--  Because of bytestring 0.9.x version, nothing provides Lazy \<-\> Strict conversion function.
--
--  written referring to bytestring-0.10.x
fromStrict' :: P.ByteString -> BL.ByteString
fromStrict' bs | S.null bs = BLI.Empty
               | otherwise = BLI.Chunk bs BLI.Empty

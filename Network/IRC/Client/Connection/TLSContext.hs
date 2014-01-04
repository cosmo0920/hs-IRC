module Network.IRC.Client.Connection.TLSContext
  ( ciphers
  , addTLSContext ) where

import Network.TLS
import Network.TLS.Extra
import qualified Crypto.Random.AESCtr as RA
import System.IO
import Data.Maybe
#if __GLASGOW_HASKELL__ <= 704
import Prelude hiding (catch)
#else
import Prelude
#endif

-- | set up cipher setting array
ciphers :: [Cipher]
ciphers =
        [ cipher_AES128_SHA1
        , cipher_AES256_SHA1
        , cipher_RC4_128_MD5
        , cipher_RC4_128_SHA1
        ]

-- | add TLS Context
addTLSContext :: Handle -> IO (Maybe Context)
addTLSContext h = do
  let params = defaultParamsClient{pCiphers = ciphers}
  g <- RA.makeSystem
  con <- contextNewOnHandle h params g
  handshake con
  return $ Just con

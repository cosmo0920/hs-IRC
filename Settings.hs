-- | Bot Settings
module Settings
  ( server
  , port
  , chan
  , nick
  , password ) where
import Data.Aeson (decode, Value)
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import Data.Text
import Control.Lens
import Data.Aeson.Lens
import Prelude

server :: IO String
server   = readSetting "server"
port :: IO Int
port     = readSetting' "port"
chan :: IO String
chan     = readSetting "channel"
nick :: IO String
nick     = readSetting "nick"
password :: IO String
password = readSetting "password"

readSetting :: String -> IO String
readSetting val = do
  fstr <- B.readFile "setting.json"
  let v = decode fstr :: Maybe Value
  let retval = v ^. key (pack val) :: Maybe String
  let _retval = fromJust retval
  return _retval

readSetting' :: String -> IO Int
readSetting' val = do
  fstr <- B.readFile "setting.json"
  let v = decode fstr :: Maybe Value
  let retval = v ^. key (pack val) :: Maybe Int
  let _retval = fromJust retval
  return _retval

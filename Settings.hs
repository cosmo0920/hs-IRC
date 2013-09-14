-- | Bot setting
module Settings
  ( server
  , port
  , chan
  , nick
  , password ) where
import Prelude
import IRC.Util

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

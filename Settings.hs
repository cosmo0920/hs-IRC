-- | Bot setting
module Settings
  ( server
  , port
  , chan
  , nick
  , password
  , realname ) where
import Prelude
import IRC.Util

server :: IO String
server   = readSetting "server"
port :: IO Int
port     = readSettingInt "port"
chan :: IO String
chan     = readSetting "channel"
nick :: IO String
nick     = readSetting "nick"
password :: IO String
password = readSetting "password"
realname :: IO String
realname = readSetting "realname"
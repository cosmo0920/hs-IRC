-- | Bot setting
module Network.IRC.Client.Settings
  ( server
  , port
  , chan
  , nick
  , password
  , realname
  , usessl ) where
import Prelude
import Network.IRC.Client.Util

server :: IO String
server   = readSetting "server"
port :: IO Int
port     = readSettingInt "port"
chan :: IO String
chan     = readSetting "channel"
nick :: IO String
nick     = readSetting "nick"
password :: IO (Maybe String)
password = readSetting' "password"
realname :: IO String
realname = readSetting "realname"
usessl :: IO Bool
usessl = readSettingBool "usessl"
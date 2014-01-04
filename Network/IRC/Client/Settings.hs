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
server   = readYamlValue "server"
port :: IO Int
port     = readYamlValue "port"
chan :: IO String
chan     = readYamlValue "channel"
nick :: IO String
nick     = readYamlValue "nick"
password :: IO (Maybe String)
password = readYamlValueMaybe "password"
realname :: IO String
realname = readYamlValue "realname"
usessl :: IO Bool
usessl = readYamlValue "usessl"
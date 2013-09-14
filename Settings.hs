-- | Bot Settings
module Settings
  ( server
  , port
  , chan
  , nick
  , password ) where
server :: String
server   = "irc.freenode.org"
port :: Int
port     = 6667
chan :: String
chan     = "#tutbot-testing"
nick :: String
nick     = "hsbot"
password :: String
password = "password"
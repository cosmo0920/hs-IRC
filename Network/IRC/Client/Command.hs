module Network.IRC.Client.Command
  ( privmsg
  , noticemsg
  , notifysendmsg
  , regexhaskell
  , askChannelTopic
  , setChannelTopic
  , passwordAuth
  , ircJoin
  , write
  , writeSerial
  ) where
import Data.List
import Network.TLS
import Network.BSD
import System.Process
import Text.Regex.Posix
import Control.Monad.Reader
import Text.Printf
import Data.Maybe
import Control.Concurrent
import Prelude hiding (catch)
import Network.IRC.Client.Type
import Network.IRC.Client.Encode
import Network.IRC.Client.Settings

-- | Send a private message to the current chan + server
privmsg :: String -> Net ()
privmsg s = do
  chan' <- liftIO $ chan
  write "PRIVMSG" (chan' ++ " :" ++ s)

-- | Send a notice message to the current chan + server
noticemsg :: String -> Net ()
noticemsg s = do
  chan' <- liftIO $ chan
  write "NOTICE" (chan' ++ " :" ++ s)

-- | Send a message out to desktop popup to use notify-send.
-- This function depends /notify-send/ command.
notifysendmsg :: String -> Net ()
notifysendmsg msg = do
  liftIO $ system $ "notify-send IRC '(H|h)askell regexp matched.\nIRC message :"++msg++"'"
  return ()

-- | when irc message contains /(H|h)askell/ , it returns true
--   otherwise, return false.
regexhaskell :: String -> Bool
regexhaskell x = do
  x =~ "(H|h)askell" :: Bool

-- | ask channel topic
askChannelTopic :: Net ()
askChannelTopic = do
  chan' <- liftIO $ chan
  write "TOPIC" chan'

-- | set channel topic
setChannelTopic :: String -> Net ()
setChannelTopic s = do
  chan' <- liftIO $ chan
  write "TOPIC" (chan' ++ " :" ++ s)

-- | execute password authentication if exists
passwordAuth :: Net ()
passwordAuth = do
  pass <- liftIO password
  if isNothing pass then
    return ()
  else
    writeSerial "PASS" (fromJust pass)

-- | join IRC
ircJoin :: Net ()
ircJoin = do
  nick' <- liftIO $ nick
  chan' <- liftIO $ chan
  real' <- liftIO $ realname
  hostname <- liftIO $ getHostName
  writeSerial "NICK" nick'
  writeSerial "USER" (nick'++" "++hostname++" * :"++real')
  writeSerial "JOIN" chan'

-- | Send a message out to the server we're currently connected to.
--   /Async/ Version.
write :: String -> String -> Net ()
write s t = do
  h <- asks socket
  mctx <- asks tlsCtx
  liftIO $ do
    forkIO $ do
      if isNothing mctx then
        hPrintf h "%s %s\r\n" s t
      else
        sendData (fromJust mctx) (fromStrict' $ packWithEncoding $ printf "%s %s\r\n" s t)
    forkIO $ printf "> %s %s\n" s t
  return ()

-- | Send a message out to the server we're currently connected to.
--  /Serial/ Version.
writeSerial :: String -> String -> Net ()
writeSerial s t = do
  h <- asks socket
  mctx <- asks tlsCtx
  liftIO $ do
    if isNothing mctx then
      hPrintf h "%s %s\r\n" s t
    else
      sendData (fromJust mctx) (fromStrict' $ packWithEncoding $ printf "%s %s\r\n" s t)
    printf "> %s %s\n" s t
  return ()

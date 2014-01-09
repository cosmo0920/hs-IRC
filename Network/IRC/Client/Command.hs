module Network.IRC.Client.Command
  ( privmsg
  , noticemsg
  , notifysendmsg
  , regexhaskell
  , askChannelTopic
  , setChannelTopic
  , setBotNickname
  , passwordAuth
  , ircJoin
  , writeSerial
  , writeMsg
  , loggingMsg ) where
import Data.List
import Network.TLS
import Network.BSD
import System.IO (Handle)
import System.Process
import System.Log.FastLogger
import Text.Regex.Posix
import Control.Monad.Reader
import Text.Printf
import Data.Maybe
#if __GLASGOW_HASKELL__ <= 704
import Prelude hiding (catch)
#else
import Prelude
#endif
import Network.IRC.Client.Type
import Network.IRC.Client.Encode
import Network.IRC.Client.Settings

-- | Send a private message to the current chan + server
privmsg :: Bot -> String -> IO ()
privmsg Bot{..} s = do
  chan' <- liftIO $ chan
  writeMsg Bot{..} "PRIVMSG" (chan' ++ " :" ++ s)

-- | Send a notice message to the current chan + server
noticemsg :: Bot -> String -> IO ()
noticemsg Bot{..} s = do
  chan' <- liftIO $ chan
  writeMsg Bot{..} "NOTICE" (chan' ++ " :" ++ s)

-- | Send a message out to desktop popup to use notify-send.
-- This function depends /notify-send/ command.
notifysendmsg :: String -> IO ()
notifysendmsg msg = do
  system $ "notify-send IRC '[Hh]askell regexp matched.\nIRC message :"++msg++"'"
  return ()

-- | when irc message contains /(H|h)askell/ , it returns true
--   otherwise, return false.
regexhaskell :: String -> Bool
regexhaskell x = do
  x =~ "[Hh]askell" :: Bool

-- | ask channel topic
askChannelTopic :: Bot -> IO ()
askChannelTopic Bot{..} = do
  chan' <- liftIO $ chan
  writeMsg Bot{..} "TOPIC" chan'

-- | set channel topic
setChannelTopic :: Bot -> String -> IO ()
setChannelTopic Bot{..} s = do
  chan' <- liftIO $ chan
  writeMsg Bot{..} "TOPIC" (chan' ++ " :" ++ s)

-- | set Bot nickname
setBotNickname :: Bot -> String -> IO ()
setBotNickname Bot{..} s = writeMsg Bot{..} "NICK" s

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
--  /Serial/ Version.
writeSerial :: String -> String -> Net ()
writeSerial s t = do
  h <- asks socket
  mctx <- asks tlsCtx
  logSet <- asks logger
  let bot = Bot { socket = h
                , tlsCtx = mctx
                , logger = logSet }
  serv <- liftIO $ server
  liftIO $ do
    writeMsg bot s t
    loggingMsg logSet (printf "> %s %s" s t)
  return ()

-- | write String To IRC Server
writeMsg :: Bot -> String -> String -> IO ()
writeMsg Bot{..} s t = do
  if isNothing tlsCtx then
    hPrintf socket "%s %s\r\n" s t
  else
    sendData (fromJust tlsCtx) (fromStrict' $ packWithEncoding $ printf "%s %s\r\n" s t)
  loggingMsg logger (printf "> %s %s" s t)

-- | logging IRC messages
loggingMsg :: LoggerSet -> String -> IO ()
loggingMsg loggerSet msg = do
  pushLogStr loggerSet (toLogStr $ msg ++ "\n")
  flushLogStr loggerSet

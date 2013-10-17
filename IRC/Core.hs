{-# OPTIONS_HADDOCK ignore-exports #-}
-- | In this code, liftIO convert type(s) as follows:
--
--   > type Net = ReaderT Bot IO
--   > liftIO :: IO a -> Net a
module IRC.Core
  ( defaultMain
  ) where
import Data.List
import Network
import Network.BSD
import Network.TLS
import System.IO
import System.Exit
import System.Process
import Text.Regex.Posix
import Control.Monad.Reader
import Control.Exception
import Text.Printf
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Control.Concurrent
import Prelude hiding (catch)
import IRC.Type
import IRC.Settings
import IRC.Encode
import IRC.Connection.TLSContext

-- |Set up actions to run on start and end, and run the main loop
defaultMain :: IO ()
defaultMain = do
  bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = catch (runReaderT run st) (\(SomeException _) -> return ())

-- | Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
  serv' <- server
  port' <- port
  h <- connectTo serv' (PortNumber (fromIntegral port'))
  hSetEncoding h utf8
  hSetBuffering h NoBuffering
  useSsl <- usessl
  tls <- if useSsl
    then addTLSContext h
    else return Nothing
  return Bot { socket = h
             , tlsCtx = tls }
    where
      notify a = bracket_
        (server >>= printf "Connecting to %s ... " >> hFlush stdout)
        (putStrLn "done.")
        a

-- | execute password authentication if exists
passwordAuth :: Net ()
passwordAuth = do
  pass <- liftIO password
  if isNothing pass then
    return ()
  else
    write "PASS" (fromJust pass)

-- | We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
  passwordAuth
  nick' <- liftIO $ nick
  chan' <- liftIO $ chan
  real' <- liftIO $ realname
  hostname <- liftIO $ getHostName
  write "NICK" nick'
  write "USER" (nick'++" "++hostname++" * :"++real')
  write "JOIN" chan'
  mctx <- asks tlsCtx
  if isNothing mctx then
    asks socket >>= listen
  else
    listenSsl (fromJust mctx)

-- | Process each line from the server
listen :: Handle -> Net ()
listen h = forever $ do
  s <- init `fmap` liftIO (hGetLine h)
  liftIO (putStrLn s)
  if ping s then pong s else eval (clean s)
    where
      forever a = a >> forever a
      clean     = drop 1 . dropWhile (/= ':') . drop 1
      ping x    = "PING :" `isPrefixOf` x
      pong x    = write "PONG" (':' : drop 6 x)

-- | Process each line from the server with ssl context
listenSsl :: TLSCtx -> Net ()
listenSsl ctx = forever $ do
  out <- recvData ctx
  liftIO (B.putStrLn out)
  if ping (B.unpack out) then
     pong (B.unpack out)
  else eval (clean (B.unpack out))
    where
      forever a = a >> forever a
      clean     = drop 1 . dropWhile (/= ':') . drop 1
      ping x    = "PING :" `isPrefixOf` x
      pong x    = write "PONG" (':' : drop 6 x)

-- | Dispatch a command
eval :: String -> Net ()
eval     "!quit-lambdabot"     = write "QUIT" ":Exiting" >> liftIO (exitWith ExitSuccess)
eval     "!lambda"             = noticemsg "λ!"
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval x | regexhaskell x        = notifysendmsg x
eval     _                     = return () -- ignore anything else.

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

-- | Send a message out to desktop popup to use notify-send
notifysendmsg :: String -> Net ()
notifysendmsg msg = do
  liftIO $ system $ "notify-send IRC '(H|h)askell regexp matched.\nIRC message :"++msg++"'"
  return ()

-- | when irc message contains /(H|h)askell/ , it returns true
--   otherwise, return false.
regexhaskell :: String -> Bool
regexhaskell x = do
  x =~ "(H|h)askell" :: Bool

-- | Send a message out to the server we're currently connected to
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

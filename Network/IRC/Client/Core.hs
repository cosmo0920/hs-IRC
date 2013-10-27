{-# OPTIONS_HADDOCK ignore-exports #-}
-- | In this code, liftIO convert type(s) as follows:
--
--   > type Net = ReaderT Bot IO
--   > liftIO :: IO a -> Net a
module Network.IRC.Client.Core
  ( defaultMain
  ) where
import Data.List
import Network
import Network.TLS
import System.IO
import System.Exit
import Control.Monad.Reader
import Control.Exception
import Text.Printf
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Prelude hiding (catch)
import Network.IRC.Client.Type
import Network.IRC.Client.Settings
import Network.IRC.Client.Connection.TLSContext
import Network.IRC.Client.Command
import Network.IRC.Client.Encode

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
  hSetNewlineMode h universalNewlineMode
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

-- | We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
  passwordAuth
  ircJoin
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
  else eval (clean (unpackWithEncoding out))
    where
      forever a = a >> forever a
      clean     = drop 1 . dropWhile (/= ':') . drop 1
      ping x    = "PING :" `isPrefixOf` x
      pong x    = write "PONG" (':' : drop 6 x)

-- | Dispatch a command
eval :: String -> Net ()
eval     "!quit-lambdabot"     = writeSerial "QUIT" ":Exiting" >> liftIO (exitWith ExitSuccess)
eval     "!lambda"             = noticemsg "λ!"
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval x | regexhaskell x        = notifysendmsg x
eval     "!topic"              = askChannelTopic
eval     _                     = return () -- ignore anything else.
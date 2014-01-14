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
import System.Log.FastLogger
import Control.Monad.Reader
import Control.Exception
import Control.Concurrent
import Text.Printf
import Data.Maybe
import Data.IORef
#if __GLASGOW_HASKELL__ <= 704
import Prelude hiding (catch)
#else
import Prelude
#endif
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
    loop st    = catch (runReaderT run st) (\(SomeException e) -> do
                                              putStrLn $ "[connection] " ++ show e
                                              return ())

-- | Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
  serv' <- server
  port' <- port
  h <- connectTo serv' (PortNumber (fromIntegral port'))
  hSetEncoding h utf8
  hSetBuffering h NoBuffering
  -- TODO: search suitable Mode
  -- such as, universalNewlineMode, noNewlineTranslation, nativeNewlineMode
  hSetNewlineMode h nativeNewlineMode
  loggerSet <- newLoggerSet defaultBufSize Nothing
  useSsl <- usessl
  tls <- if useSsl
    then addTLSContext h
    else return Nothing
  return Bot { socket = h
             , tlsCtx = tls
             , logger = loggerSet }
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
listen h = do
  logSet <- asks logger
  let bot = Bot { socket = h
                , tlsCtx = Nothing
                , logger = logSet }
  serv <- liftIO $ server
  pingref <- liftIO $ newIORef False
  liftIO $ forever $ do
    s <- init `fmap` liftIO (hGetLine h)
    let s' = filter (`notElem` "\r\n") s
    loggingMsg logSet s'
    void . forkIO $ handleMsg bot pingref s'
  liftIO $ void . forkIO $ handlePing bot pingref serv

-- | Process each line from the server with ssl context
listenSsl :: TLSCtx -> Net ()
listenSsl ctx = do
  logSet <- asks logger
  sock <- asks socket
  let bot = Bot { socket = sock
                , tlsCtx = (Just ctx)
                , logger = logSet }
  serv <- liftIO $ server
  pingref <- liftIO $ newIORef False
  liftIO $ forever $ do
    out <- recvData ctx
    let msg = unpackWithEncoding out
    loggingMsg logSet msg
    void . forkIO $ handleMsg bot pingref msg
  liftIO $ void . forkIO $ handlePing bot pingref serv

-- | handle ping/pong
handlePing :: Bot -> IORef Bool -> String -> IO ()
handlePing Bot{..} pingref servn = do
  writeIORef pingref False
  writeMsg Bot{..} "PING" servn
  threadDelay 12000000
  pong <- readIORef pingref
  if pong then do
    handlePing Bot{..} pingref servn
  else
    return ()

-- | send String to IRC
handleMsg :: Bot -> IORef Bool -> String -> IO ()
handleMsg Bot{..} pingref str = do
  if ping str then do
     writeIORef pingref True
     pong str
  else
     eval Bot{..} (clean str)
    where
      clean     = drop 1 . dropWhile (/= ':') . drop 1
      ping x    = "PING :" `isPrefixOf` x
      pong x    = writeMsg Bot{..} "PONG" (':' : drop 6 x)

-- | Dispatch a command
eval :: Bot -> String -> IO ()
eval Bot{..}    "!quit-lambdabot"      = writeMsg Bot{..} "QUIT" ":Exiting" >> liftIO (exitWith ExitSuccess)
eval Bot{..}     "!lambda"             = noticemsg Bot{..} "λ!"
eval Bot{..} x | "!id " `isPrefixOf` x = privmsg Bot{..} (drop 4 x)
eval Bot{..} x | regexhaskell x        = notifysendmsg x
eval Bot{..}     "!topic"              = askChannelTopic Bot{..}
eval Bot{..}     _                     = return () -- ignore anything else.

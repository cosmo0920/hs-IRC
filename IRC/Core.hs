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
import System.IO
import System.Exit
import System.Process
import Text.Regex.Posix
import Control.Monad.Reader
import Control.Exception
import Text.Printf
import Prelude hiding (catch)
import Type
import Settings

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
  hSetBuffering h NoBuffering
  return (Bot h)
    where
      notify a = bracket_
        (server >>= printf "Connecting to %s ... " >> hFlush stdout)
        (putStrLn "done.")
        a

-- | We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
  -- write "PASS" password
  nick' <- liftIO $ nick
  chan' <- liftIO $ chan
  write "NICK" nick'
  write "USER" (nick'++" 0 * :haskell bot")
  write "JOIN" chan'
  asks socket >>= listen

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

-- | Dispatch a command
eval :: String -> Net ()
eval     "!quit-lambdabot"     = write "QUIT" ":Exiting" >> liftIO (exitWith ExitSuccess)
eval     "!lambda"             = noticemsg "λ!"
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval     x                     = regexhaskell x -- match regexp or ignore everything.

-- | Send a privmsg to the current chan + server
privmsg :: String -> Net ()
privmsg s = do
  chan' <- liftIO $ chan
  write "PRIVMSG" (chan' ++ " :" ++ s)

-- | Send a privmsg to the current chan + server
noticemsg :: String -> Net ()
noticemsg s = do
  chan' <- liftIO $ chan
  write "NOTICE" (chan' ++ " :" ++ s)

-- | Send a message out to desktop popup to use notify-send
notifysendmsg :: String -> Net ()
notifysendmsg s = do
  _ <- liftIO $ system s
  return ()

-- | when irc message contains /(H|h)askell/ , it executes notify-send to notify
regexhaskell :: String -> Net ()
regexhaskell x = do
  if (x =~ "(H|h)askell" :: Bool) then
    notifysendmsg "notify-send IRC (H|h)askell regexp matched'"
  else
    return () -- ignore everything else

-- | Send a message out to the server we're currently connected to
write :: String -> String -> Net ()
write s t = do
  h <- asks socket
  liftIO $ hSetEncoding h utf8
  liftIO $ hPrintf h "%s %s\r\n" s t
  liftIO $ printf    "> %s %s\n" s t

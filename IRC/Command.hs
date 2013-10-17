module IRC.Command
  ( privmsg
  , noticemsg
  , notifysendmsg
  , regexhaskell
  , write
  ) where
import Data.List
import Network.TLS
import System.Process
import Text.Regex.Posix
import Control.Monad.Reader
import Text.Printf
import Data.Maybe
import Control.Concurrent
import Prelude hiding (catch)
import IRC.Type
import IRC.Encode
import IRC.Settings

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

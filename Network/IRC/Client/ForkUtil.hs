module Network.IRC.Client.ForkUtil
  ( Network.IRC.Client.ForkUtil.forkFinally
  -- re-export
  , module Control.Concurrent ) where
import Prelude

import Control.Exception.Base as Exception
import Control.Concurrent

-- | base 4.5.*'s Control.Concurrent package does not export forkFinally function.
--
--   So, implement forkFinally.
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then
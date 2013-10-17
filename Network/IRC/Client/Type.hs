module Network.IRC.Client.Type where
import Control.Monad.Reader
import System.IO
import Network.TLS
import Data.Maybe

-- | The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle
               , tlsCtx :: Maybe TLSCtx }

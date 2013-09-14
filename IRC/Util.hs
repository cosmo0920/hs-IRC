{-# OPTIONS_HADDOCK ignore-exports #-}
-- | using aeson-lens
--
--   > readSetting "server" :: IO String
--   > readSettingInt "port" :: IO Int
module IRC.Util
  ( readSetting
  , readSettingInt ) where

import Data.Aeson (decode, Value)
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import Data.Text
import Control.Lens
import Data.Aeson.Lens
import Prelude

-- | read string value from setting.json
readSetting :: String -> IO String
readSetting val = do
  fstr <- B.readFile "setting.json"
  let v = decode fstr :: Maybe Value
  let retval = v ^. key (pack val) :: Maybe String
  let _retval = fromJust retval
  return _retval

-- | read Integer value from setting.json
readSettingInt :: String -> IO Int
readSettingInt val = do
  fstr <- B.readFile "setting.json"
  let v = decode fstr :: Maybe Value
  let retval = v ^. key (pack val) :: Maybe Int
  let _retval = fromJust retval
  return _retval
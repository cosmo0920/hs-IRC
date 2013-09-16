{-# OPTIONS_HADDOCK ignore-exports #-}
-- | using aeson-lens
--
--   > readSetting "server" :: IO String
--   > readSettingInt "port" :: IO Int
module IRC.Util
  ( readSetting
  , readSetting'
  , readSettingInt ) where

import Data.Aeson (decode, Value)
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import Data.Text
import Control.Lens
import Data.Aeson.Lens
import Prelude

-- | read from setting.json and return String
readSetting :: String -> IO String
readSetting val = do
  retval <- readSetting' val
  let _retval = fromJust retval
  return _retval

-- | read from setting.json and return Maybe String
readSetting' :: String -> IO (Maybe String)
readSetting' val = do
  fstr <- B.readFile "setting.json"
  let v = decode fstr :: Maybe Value
  let retval = v ^. key (pack val) :: Maybe String
  return retval

-- | read from setting.json and return Int
readSettingInt :: String -> IO Int
readSettingInt val = do
  fstr <- B.readFile "setting.json"
  let v = decode fstr :: Maybe Value
  let retval = v ^. key (pack val) :: Maybe Int
  let _retval = fromJust retval
  return _retval
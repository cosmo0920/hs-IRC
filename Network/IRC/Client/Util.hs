{-# OPTIONS_HADDOCK ignore-exports #-}
-- | using aeson-lens
--
--   > readSetting "server" :: IO String
--   > readSetting' "password" :: IO (Maybe String)
--   > readSettingInt "port" :: IO Int
module Network.IRC.Client.Util
  ( readSetting
  , readSetting'
  , readSettingInt
  , readSettingBool ) where

import Data.Aeson (decode, Value)
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Yaml as Y
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
  v <- readYamlFile
  let retval = v ^. key (pack val) :: Maybe String
  return retval

-- | read from setting.json and return Int
readSettingInt :: String -> IO Int
readSettingInt val = do
  v <- readYamlFile
  let retval = v ^. key (pack val) :: Maybe Int
  let _retval = fromJust retval
  return _retval

-- | read from setting.json and return Bool
readSettingBool :: String -> IO Bool
readSettingBool val = do
  v <- readYamlFile
  let retval = v ^. key (pack val) :: Maybe Bool
  let _retval = fromJust retval
  return _retval

-- | read setting from yaml
readYamlFile :: IO (Maybe Value)
readYamlFile = do
  fstr <- B.readFile settingFile
  let v = Y.decode fstr :: Maybe Value
  return v

-- | set setting file name
settingFile :: String
settingFile = "setting.yml"
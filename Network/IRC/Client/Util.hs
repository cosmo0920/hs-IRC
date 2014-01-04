{-# OPTIONS_HADDOCK ignore-exports #-}
-- | using aeson-lens
--
--   > readYamlValue "server" :: IO String
--   > readYamlValue "port" :: IO Int
--   > readYamlValueMaybe "password" :: IO (Maybe String)
module Network.IRC.Client.Util
  ( readYamlValue
  , readYamlValueMaybe ) where

import Data.Maybe
import qualified Data.ByteString as B
import Data.Yaml as Y
import Data.Text
import Control.Lens
import Data.Aeson.Lens
import Prelude

-- | read from setting.json and return (ToJSON v, FromJSON v) => IO (Maybe v)
readYamlValueMaybe :: (ToJSON v, FromJSON v) => String -> IO (Maybe v)
readYamlValueMaybe val = do
  v <- readYamlFile
  let _retval = v ^. key (pack val)
  return _retval

-- | read from setting.json and return (ToJSON v, FromJSON v) => IO v
readYamlValue :: (ToJSON v, FromJSON v) => String -> IO v
readYamlValue val = do
  retval <- readYamlValueMaybe val
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
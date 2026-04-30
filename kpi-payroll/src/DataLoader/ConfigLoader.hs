{-# LANGUAGE OverloadedStrings #-}

module DataLoader.ConfigLoader
  ( loadConfig
  )
where

import qualified Data.Aeson as A
import Data.Aeson (FromJSON (parseJSON), eitherDecodeFileStrict')
import Core.Common (AppError (..))
import Core.Types (BonusTier (..), Config (..))
import System.Directory (doesFileExist)

instance FromJSON BonusTier where
  parseJSON = A.withObject "BonusTier" $ \o ->
    BonusTier <$> o A..: "threshold" <*> o A..: "bonusPercent"

instance FromJSON Config where
  parseJSON = A.withObject "Config" $ \o ->
    Config
      <$> o A..: "baseKpi"
      <*> o A..: "bonusScale"
      <*> o A..: "taxRate"
      <*> o A..: "socialRate"

loadConfig :: FilePath -> IO (Either AppError Config)
loadConfig fp = do
  exists <- doesFileExist fp
  if not exists
    then pure $ Left (FileNotFound fp)
    else do
      decoded <- eitherDecodeFileStrict' fp
      pure $ either (Left . ParseError) Right decoded

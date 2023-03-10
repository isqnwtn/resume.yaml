{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Info
  ( Resume(..)
  , PersonalInfo(..)
  , Experience(..)
  , Duration(..)
  )
where

import Data.Text
import Data.Yaml
import GHC.Generics

data Resume = Resume
  { personalInfo :: PersonalInfo
  , workExperience :: [Experience]
  , projects :: [Experience]
  , education :: [Experience]
  } deriving (Show,Generic)

instance FromJSON Resume

data PersonalInfo = PersonalInfo
  { name :: Text
  , position :: Text
  , description :: Text
  } deriving (Show, Generic)

instance FromJSON PersonalInfo

data Experience = Experience
  { duration :: Maybe (Duration Text)
  , title    :: Text
  , achievements :: [Text]
  } deriving (Show, Generic)

instance FromJSON Experience

data Duration a = Duration
  { startDate :: a
  , endDate   :: Maybe a
  } deriving (Show, Generic)

instance (FromJSON a) => FromJSON (Duration a) where
  parseJSON (Object v) = do
    startDate <- v .: "startDate"
    endDate   <- v .:? "endDate"
    return (Duration startDate endDate)
  parseJSON _ = mempty

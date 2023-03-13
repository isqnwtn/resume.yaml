{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Info
  ( Resume(..)
  , PersonalInfo(..)
  , Experience(..)
  , Duration(..)
  , Work(..)
  , Education(..)
  , Project(..)
  )
where

import Data.Text
import Data.Yaml
import GHC.Generics

data Resume = Resume
  { personalInfo :: PersonalInfo
  , workExperience :: [Experience Work]
  , projects :: [Experience Project]
  , education :: [Experience Education]
  } deriving (Show,Generic)

instance FromJSON Resume

data PersonalInfo = PersonalInfo
  { name :: Text
  , title :: Text
  , description :: Text
  } deriving (Show, Generic)

instance FromJSON PersonalInfo

data Experience a = Experience
  { duration :: Maybe (Duration Text)
  , spec    :: a
  , achievements :: [Text]
  } deriving (Show, Generic)

instance FromJSON (Experience Work)
instance FromJSON (Experience Education)
instance FromJSON (Experience Project)

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

data Work = Work
  { workPlaceName :: Text
  , workLocation  :: Text
  , position      :: Text
  } deriving (Show, Generic)

instance FromJSON Work

data Education = Education
  { universityName :: Text
  , universityPlace :: Text
  } deriving (Show, Generic)

instance FromJSON Education

data Project = Project
  { projectName :: Text
  , projectLanguage :: Text
  } deriving (Show, Generic)
instance FromJSON Project

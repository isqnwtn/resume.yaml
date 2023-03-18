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

import Data.Text (Text,pack)
import Data.List.Split
import Data.Yaml
import GHC.Generics

data Resume = Resume
  { personalInfo :: PersonalInfo
  , workExperience :: [Experience Work]
  , projects :: [Experience Project]
  , educationInfo :: [Experience Education]
  , skills :: [(Text,Int)]
  } deriving (Show,Generic)

instance FromJSON Resume where
  parseJSON (Object v) = do
    pI <- v .: "personalInfo"
    wE <- v .: "workExperience"
    prj <- v .: "projects"
    edu <- v .: "education"
    sk <- v .: "skills"
    return $ Resume pI wE prj edu (convertSkills sk)
      where
        splitIt s = (\x -> ( pack $ head x, read $ head $ tail x)) (splitOn "@" s)
        convertSkills ls = map splitIt ls
  parseJSON _ = mempty

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
  , degreeName :: Text
  , cgpa :: Float
  } deriving (Show, Generic)

instance FromJSON Education

data Project = Project
  { projectName :: Text
  , projectLanguage :: Text
  } deriving (Show, Generic)
instance FromJSON Project

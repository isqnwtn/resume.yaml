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
  , URL(..)
  )
where

import Data.Text (Text,pack,splitOn)
import qualified Data.List.Split as S
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
        convertSkills ls = map splitIt ls
  parseJSON _ = mempty

splitIt :: Read a => String -> (Text,a)
splitIt s = (\x -> ( pack $ head x, read $ head $ tail x)) (S.splitOn "@" s)

data URL = URL{
   urlName :: Text
  ,urlLink :: Text
              } deriving(Show,Generic)
instance FromJSON URL where
  parseJSON (String x) = return $ URL name link
    where
      (name,link) = splitA x
  parseJSON _ = mempty

splitA :: Text -> (Text,Text)
splitA s = (\x -> (head x,head $ tail x)) (splitOn (pack "@") s)

data PersonalInfo = PersonalInfo
  { name :: Text
  , title :: Text
  , description :: Text
  , photo :: Maybe Text

  , phoneNum :: Text
  , email :: Text
  , location :: Text
  , website :: URL
  , github :: URL
  , linkedIn :: URL
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
  , projectCategory :: Text
  , projectRepo :: URL
  } deriving (Show, Generic)
instance FromJSON Project

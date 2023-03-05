-- |

module Info where

import Data.Text

data Resume = Resume
  { presonalInfo :: Text
  , workExperience :: [Experience Text]
  , projects :: [Experience Text]
  , education :: [Experience Text]
  }

data Experience a = Exp
  { duration :: Maybe (Duration Text)
  , title    :: Text
  , achievements :: [Text]
  }

data Duration a = Duration
  { startDate :: a
  , endDate   :: Maybe a
  }

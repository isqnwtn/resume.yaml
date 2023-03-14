{-# LANGUAGE RecordWildCards #-}

module Gen.Body
  ( resumeBody )
where

import Gen.Common
import Gen.Work
import Gen.Skills
import Latex
import Info
import Lib
import Data.Text (unpack)

resumeBody :: Resume -> Latex Ltx
resumeBody Resume{..} =
  let left = workExp workExperience
      right = renderSkills skills
  in table (1.0,1.0) "white" left right


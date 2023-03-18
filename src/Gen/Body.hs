{-# LANGUAGE RecordWildCards #-}

module Gen.Body
  ( resumeBody )
where

import Gen.Common
import Gen.Work
import Gen.Skills
import Gen.Education
import Latex
import Info
import Lib

resumeBody :: Resume -> Latex Ltx
resumeBody Resume{..} =
  let left = (workExp workExperience)
       :#>> (sle $ Slash "vspace" :<@> Curl "0.2cm")
       :#>> (education educationInfo)
      right = renderSkills skills
  in table (1.0,1.0) "white" left right


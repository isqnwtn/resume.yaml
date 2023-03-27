-- |

module Gen.Skills
 ( renderSkillsPie
 , renderSkills
 )where

import Lib
import Gen.Common
import Data.Text (unpack, Text)
import Latex

renderSkillsPie :: [(Text,Int)] -> Latex Ltx
renderSkillsPie sk
  = (sle $ Slash "section*" :<@> Curl "SKILLS")
  :#>> ( Cld (BegEnd "tikzpicture" ELine) :<&>
         ( Cld (InCurl (Str "pie"))
           :<^> (
             concatLtx $ map renderSkillPie newsk
                )
         )
       )
    where
      total = sum $ map snd sk
      newsk = map ( \(x,y) -> (x, ((fromIntegral y)*100.0/(fromIntegral total) ) ) ) sk

renderSkillPie :: (Text,Float) -> Latex Ltx
renderSkillPie (skill,amt)
  = sle $ (Str (show amt))
  :<@> (Str "/")
  :<@> (Str $ unpack skill)
  :<@> (Str ",")

renderSkills :: [(Text,Int)] -> Latex Ltx
renderSkills sk
 = (sle $ Slash "section*" :<@> Curl "SKILLS")
  :#>> (sle $ Slash "large")
  :#>> (sle $ Slash "bfseries")
  :#>> ( Cld (BegEnd "tabular" (Curl "l r")) :<&>
          (concatLtx $ (map renderOne sk))
       )
  :#>> (sle $ Slash "normalsize")
    where
      maxLevel = maximum $ map snd sk
      renderOne (name,level)
        = (sle $ Str (unpack name))
          :#>> (sle $ Str "&")
          :#>> (sle $ Str $ concat $
               (replicate (maxLevel - level) "\\faStar[regular]")
               <>
                (replicate level "\\faStar")
               )
          :#>> (sle $ Slash "\\")

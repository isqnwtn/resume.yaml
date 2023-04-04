-- |

module Gen.Skills
 ( renderSkillsPie
 , renderSkills
 )where

import Lib
import Gen.Common
import Data.Text (unpack, Text, pack)
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
  :#>> ( Cld (BegEnd "tabular" (Curl "l r l r")) :<&>
          (concatLtx $ (map renderTwo $ splitAndZip (Just <$> sk) Nothing))
       )
  :#>> (sle $ Slash "normalsize")
    where
      maxLevel = 5
      createStars empty full = (replicate empty "\\faStar[regular]") <> (replicate full "\\faStar")
      renderOneSkill (Just (name,level))
        = (sle $ Str (unpack name))
          :#>> (sle $ Str "&")
          :#>> (sle $ Str $ concat $ createStars (maxLevel - level) level)
      renderOneSkill Nothing
        = (sle $ Str "") :#>> (sle $ Str "&") :#>> (sle $ Str "")
      renderTwo (s1,s2) =
        (renderOneSkill s1) :#>> (sle $ Str "&") :#>> (renderOneSkill s2) :#>> (sle $ Slash "\\")

splitAndZip :: [a] -> a -> [(a,a)]
splitAndZip list extraElem  = if listlen `mod` 2 == 0
        then zip (take half list) (drop half list)
        else zip (take (half+1) list) ((drop (half+1) list) <> [extraElem])
        where
          listlen = length list
          half = (length list) `div` 2

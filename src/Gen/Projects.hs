{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Gen.Projects
 ( projectSection )where

import Data.Text (unpack,Text)
import Gen.Common
import Info
import Lib
import Latex

projectSection :: [Experience Project] -> Latex Ltx
projectSection expirience
   = (sle $ Slash "section*" :<@> Curl "PROJECTS")
     :#>> (concatLtx $ map singleExp expirience)

expContent :: Experience Project -> Latex Ltx
expContent Experience{..}
  = (sle $ Slash "newline")
  :#>> (l (unpack $ projectLanguage spec) (unpack $ projectRepo spec))
    where
      l lang repo = (sle $ Str $ lang )
          :#>> (sle $ Slash "hfill")
          :#>> (sle $ Str ("repo: " <> repo))

achievementsList :: [Text] -> Latex Ltx
achievementsList l
  = ( Cld (BegEnd "itemize" ELine) :<&>
     ( concatLtx $ map toItem l)
     )
    where
      toItem x = sle $ Slash $ "item " <> unpack x

singleExp :: Experience Project -> Latex Ltx
singleExp e@Experience{..}
  = (sle $ Str $ "% begin edu" <> (unpack $ projectName spec))
        :#>> LX (Opn (DefineColor "DarkBlue" (0.0,0.0,0.3)))
        :#>> (sle $ Slash "Large")
        -- degree name
        :#>> (sle $ Slash "color" :<@> Curl "DarkBlue")
        :#>> (sle $ Slash "textbf" :<@> (Curl $ unpack $ projectName spec))
        :#>> (sle $ Slash "vspace" :<@> Curl "0.1cm")
        :#>> (sle $ Slash "newline")

        -- university name
        :#>> (sle $ Slash "color" :<@> Curl "darkgray")
        :#>> (sle $ Slash "large")
        :#>> (sle $ Str $ unpack $ "category: " <> projectCategory spec)
        :#>> (sle $ Slash "normalsize")

        -- duration and cgpa
        :#>> (Cld (InCurl (Str "textcolor" :<@> Curl "gray"))
                :<^> (Cld (BegEnd "slshape" ELine) ) :<&> (expContent e)
                )

        -- achievements
        :#>> (sle $ Slash "color" :<@> Curl "Black")
        :#>> (sle $ Slash "upshape")
        :#>> (achievementsList achievements)
        :#>> (sle $ Slash "vspace" :<@> Curl "0.4cm")
        :#>> (sle $ Str $ "% end edu" <> (unpack $ projectName spec))

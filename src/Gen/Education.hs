{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Gen.Education
 ( education )where

import Data.Text (unpack,Text)
import Gen.Common
import Info
import Lib
import Latex

education :: [Experience Education] -> Latex Ltx
education expirience
   = (sle $ Slash "section*" :<@> Curl "EDUCATION")
     :#>> (concatLtx $ map singleExp expirience)

expContent :: Experience Education -> Latex Ltx
expContent e@Experience{..}
  = (sle $ Slash "bfseries")
  :#>> (sle $ Slash "newline")
  :#>> durationPlace
    where
      durationPlace = case duration of
        Nothing -> (sle $ Slash "hfill" :<@> Str ( show $ cgpa spec) )
        Just (Duration s Nothing) -> l (unpack s) "present" (cgpa spec)
        Just (Duration s (Just e)) -> l (unpack s) (unpack e) (cgpa spec)
      l start end c = (sle $ Str (start <> " $\\Rightarrow$ " <> end ) )
          :#>> (sle $ Slash "hfill")
          :#>> (sle $ Str ("cgpa: " <> (show c)))

achievementsList :: [Text] -> Latex Ltx
achievementsList l
  = ( Cld (BegEnd "itemize" ELine) :<&>
     ( concatLtx $ map toItem l)
     )
    where
      toItem x = sle $ Slash $ "item " <> unpack x

singleExp :: Experience Education -> Latex Ltx
singleExp e@Experience{..}
  = (sle $ Str $ "% begin edu" <> (unpack $ universityName spec))
        :#>> LX (Opn (DefineColor "DarkBlue" (0.0,0.0,0.3)))
        :#>> (sle $ Slash "Large")
        -- degree name
        :#>> (sle $ Slash "color" :<@> Curl "DarkBlue")
        :#>> (sle $ Slash "textbf" :<@> (Curl $ unpack $ degreeName spec))
        :#>> (sle $ Slash "vspace" :<@> Curl "0.1cm")
        :#>> (sle $ Slash "newline")

        -- university name
        :#>> (sle $ Slash "color" :<@> Curl "darkgray")
        :#>> (sle $ Slash "large")
        :#>> (sle $ Str $ unpack $ universityName spec <> " " <> universityPlace spec)
        :#>> (sle $ Slash "normalsize")

        -- duration and cgpa
        :#>> (Cld (InCurl (Str "textcolor" :<@> Curl "gray"))
                :<^> (Cld (BegEnd "slshape" ELine) ) :<&> (expContent e)
                )

        -- achievements
        :#>> (sle $ Slash "color" :<@> Curl "Black")
        :#>> (achievementsList achievements)
        :#>> (sle $ Str $ "% end edu" <> (unpack $ universityPlace spec))

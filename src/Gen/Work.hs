{-# LANGUAGE RecordWildCards #-}
module Gen.Work
 ( workExp
 )where

import Data.Text (unpack,Text)
import Gen.Common
import Info
import Lib
import Latex

workExp :: [Experience Work] -> Latex Ltx
workExp expirience
   = (sle $ Slash "section*" :<@> Curl "Work Experience")
     :#>> (concatLtx $ map singleExp expirience)

expContent :: Experience Work -> Latex Ltx
expContent e@Experience{..}
  = (sle $ Slash "bfseries")
  :#>> (sle $ Slash "newline")
  :#>> durationPlace
    where
      durationPlace = case duration of
        Nothing -> (sle $ Slash "hfill" :<@> Str ( unpack $ workLocation spec) )
        Just (Duration s Nothing) ->
          (sle $ Str (unpack s <> " $\\Rightarrow$ " <> "present" ) )
          :#>> (sle $ Slash "hfill")
          :#>> (sle $ Str $ unpack $ workLocation spec)
        Just (Duration s (Just end)) ->
          (sle $ Str (unpack s <> " $\\Rightarrow$ " <> unpack end))
          :#>> (sle $ Slash "hfill")
          :#>> (sle $ Str $ unpack $ workLocation spec)

achievementsList :: [Text] -> Latex Ltx
achievementsList l
  = ( Cld (BegEnd "itemize" ELine) :<&>
     ( concatLtx $ map toItem l)
     )
    where
      toItem x = sle $ Slash $ "item " <> unpack x

singleExp :: Experience Work -> Latex Ltx
singleExp e@Experience{..}
  = (sle $ Str $ "% begin exp" <> (unpack $ workPlaceName spec))
        :#>> LX (Opn (DefineColor "DarkBlue" (0.0,0.0,0.3)))
        :#>> (sle $ Slash "Large")
        -- position
        :#>> (sle $ Slash "color" :<@> Curl "DarkBlue")
        :#>> (sle $ Slash "textbf" :<@> (Curl $ unpack $ position spec))
        :#>> (sle $ Slash "vspace" :<@> Curl "0.1cm")
        :#>> (sle $ Slash "newline")

        -- work place name
        :#>> (sle $ Slash "color" :<@> Curl "darkgray")
        :#>> (sle $ Slash "large")
        :#>> (sle $ Str $ unpack $ workPlaceName spec)
        :#>> (sle $ Slash "normalsize")

        -- duration and place
        :#>> (Cld (InCurl (Str "textcolor" :<@> Curl "gray"))
                :<^> (Cld (BegEnd "slshape" ELine) ) :<&> (expContent e)
                )

        -- achievements
        :#>> (sle $ Slash "color" :<@> Curl "Black")
        :#>> (achievementsList achievements)
        :#>> (sle $ Slash "vspace" :<@> Curl "0.3cm")
        :#>> (sle $ Str $ "% end exp" <> (unpack $ position spec))

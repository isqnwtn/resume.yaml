{-# LANGUAGE RecordWildCards #-}

module Gen.Body
  ( resumeBody )
where

import Gen.Common
import Latex
import Info
import Lib
import Data.Text (unpack,Text)

resumeBody :: Resume -> Latex Ltx
resumeBody rsm@Resume{..} =
  let tab = ( Cld $ Tabularx $ Curl "\\linewidth" :<@> tabattr ) :<&> tableBody rsm

      tabhsize :: Double -> String
      tabhsize x = render $ Curl $ render $ Slash "hsize" :<@> Str ("="<>show x) :<@> Slash "hsize"

      tabattr = Curl $ "|>" <> tabhsize 1.0 <> "X|>" <> tabhsize 1.0 <> "X|"

      lenset = (sle $ Slash "setlength" :<@> Curl "\\extrarowheight" :<@> Curl "2pt")
          :#>> (sle $ Slash "setlength" :<@> Curl "\\arrayrulewidth" :<@> Curl "10pt")
      arrayrulecolor = sle $ Slash "arrayrulecolor" :<@> Curl "white"
      noindent = sle $ Slash "noindent"
  in (noindent :#>> lenset :#>> arrayrulecolor :#>> tab)

tableBody :: Resume -> Latex Ltx
tableBody Resume{..} = workExp workExperience
       :#>> (sle $ Str "&")
       :#>> (sle $ Str "meow meow")
       :#>> Empty

workExp :: [Experience Work] -> Latex Ltx
workExp expirience
  = (sle $ Slash "hline")
     :#>> (sle $ Slash "section*" :<@> Curl "Work Experience")
     :#>> (concatLtx $ map singleExp expirience)

expContent :: Experience Work -> Latex Ltx
expContent e@Experience{..}
  = (sle $ Slash "bfseries")
  :#>> (sle $ Str $ unpack $ workPlaceName spec)
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
  = (Cld (BegEnd "itshape" ELine) :<&>
     ( Cld (BegEnd "itemize" ELine) :<&>
     ( concatLtx $ map toItem l)
     )
    )
    where
      toItem x = sle $ Slash $ "item " <> unpack x

singleExp :: Experience Work -> Latex Ltx
singleExp e@Experience{..}
  = (sle $ Str $ "% begin exp" <> (unpack $ workPlaceName spec))
        :#>> (sle $ Slash "Large")
        :#>> (sle $ Slash "textbf" :<@> (Curl $ unpack $ position spec))
        :#>> (sle $ Slash "normalsize")
        :#>> (sle $ Slash "newline")
        :#>> (Cld (InCurl (Str "textcolor" :<@> Curl "gray"))
                :<^> (Cld (BegEnd "slshape" ELine) ) :<&> (expContent e)
                )
        :#>> (achievementsList achievements)
        :#>> (sle $ Str $ "% end exp" <> (unpack $ position spec))

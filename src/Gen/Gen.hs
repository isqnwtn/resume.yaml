{-# LANGUAGE RecordWildCards #-}

module Gen.Gen
 (someFunc)
where

import Gen.Common
import Gen.Body
import Latex
import Info
import Lib
import Data.Text (unpack)

someFunc :: Resume -> IO String
someFunc resume@Resume{..} = do
  let body = (Cld Document) :<&> ( (header personalInfo) :#>> resumeBody resume) :: Latex Ltx
  let li = toLines $ LX (Opn (DocClass "article"))
        :#>> pkg [] "graphicx"
        :#>> pkg ["legalpaper","margin=0in"] "geometry"
        :#>> pkg ["svgnames","table"] "xcolor"
        :#>> pkg [] "tabularx"
        :#>> pkg [] "tikz"
        :#>> pkg [] "hyperref"
        :#>> body
  let lns = map render li
  return $ unlines lns


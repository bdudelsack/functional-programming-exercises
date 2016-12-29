{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

import Graphics

graphic :: Graphic
graphic = rectangle 10.0 10.0 <> circle 5.0 <> rectangle 20.0 20.0 <> circle 10.0

main :: IO ()
main = writeFile "graphic.svg" (toSVG (colored Green graphic))

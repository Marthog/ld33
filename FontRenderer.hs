module FontRenderer(
    multiline
) where


import Graphics.Gloss


multiline :: [String] -> Picture
multiline ls = pictures $ zipWith (\a b -> translate 0 (-130*b) (text a) ) ls [0..]




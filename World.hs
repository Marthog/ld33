module World
    (decodeTile
    ,IRectangle
    ,Tile(..)
    ,World(..)
    ,loadWorld
    ,distanceSq
    ,index
    ,getTile
) where


import Graphics.Gloss
import Graphics.Gloss.Game
import Text.Parsec
import Control.Monad

import Constants
import GUI



--generateWorld



loadWorld :: String -> IO World
loadWorld mapName = do
    file <- readFile $ "map/"++mapName++".map"
    let (init:body) = lines file
    let ws = words `map` body
    tiles <- forM ws (\l -> forM l decodeTile)
    -- check if the lengths are equal
    let check = all (\x -> length x == length (head tiles)) tiles
    return $ 
        World { mapHeight = length tiles
        , mapWidth  = length $ head tiles
        , tiles     = tiles
        }



distanceSq (x0, y0) (x1, y1) = (x0-x1)^2 + (y0-y1)^2


type IRectangle = (Int, Int, Int, Int)

initRectangle centerX centerY =
    (centerX - wtiles
    ,centerY-htiles
    ,centerX+wtiles
    ,centerY+htiles)
    where   tileDiv s = (s `div` tileSize `div` 2)+1
            wtiles = tileDiv windowWidth
            htiles = tileDiv (windowHeight-100)

decodeTile :: String -> IO Tile
decodeTile word
        | word=="color:black"   = return $ Tile "black" $ color black $ rectangleSolid 20 20
        | word=="color:red"     = return $ Tile "red" $ color red $ rectangleSolid 20 20
        | otherwise             = return $ Tile word (png $ "data/"++word++".png")



getTile :: World -> Int -> Int -> Maybe Tile
getTile world x y = do
    row <- tiles world `index` y 
    row `index` x


index :: [a] -> Int -> Maybe a
index (a:_s) 0 = Just a
index (_:as) n = index as (n-1)
index _ _      = Nothing


data Tile = Tile String Picture

data World = World
    { mapWidth      :: !Int
    , mapHeight     :: !Int
    , tiles         :: [[Tile]]
    }



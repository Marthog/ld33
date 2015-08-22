module World
    (decodeTile
    ,Position,Rectangle
    ,Tile(..)
    ,tilePic
    ,World(..)
    ,loadWorld
    ,distanceSq
) where


import Graphics.Gloss
import Text.Parsec
import Control.Monad


import Constants




loadWorld :: String -> IO World
loadWorld mapName = do
    file <- readFile $ "map/"++mapName++".map"
    let (init:body) = lines file
    let tiles = ((map decodeTile) . words) `map` body
    -- check if the lengths are equal
    let check = all (\x -> length x == length (head tiles)) tiles
    return $ 
        World { mapHeight = length tiles
        , mapWidth  = length $ head tiles
        , tiles     = tiles
        }



type Position = (Float, Float)
distanceSq (x0, y0) (x1, y1) = (x0-x1)^2 + (y0-y1)^2


type Rectangle = (Int, Int, Int, Int)

initRectangle centerX centerY =
    (centerX - wtiles
    ,centerY-htiles
    ,centerX+wtiles
    ,centerY+htiles)
    where   tileDiv s = (s `div` tileSize `div` 2)+1
            wtiles = tileDiv windowWidth
            htiles = tileDiv windowHeight

decodeTile :: String -> Tile
decodeTile word = Tile { tilePic = color red $ rectangleSolid 20 20 }

data Tile = Tile
    { tilePic       :: Picture}


data World = World
    { mapWidth      :: !Int
    , mapHeight     :: !Int
    , tiles         :: [[Tile]]
    }


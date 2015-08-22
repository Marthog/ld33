import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Game
import Data.Fixed
import Data.List(elemIndex)
import Data.Maybe


import World
import Constants
import Npc


main = do
    let
        window = InWindow "you are the monster" (100, 100) (windowWidth, windowHeight)

    game <- newGame "test"

    playIO window black 60 game draw input update
    




data Game = Game
    { time          :: !Float
    , npcs          :: [Npc]
    , world           :: World

    -- x0 y0 x1 y1
    , rectangle     :: !Rectangle
    }


initRectangle centerX centerY =
    (centerX - wtiles
    ,centerY-htiles
    ,centerX+wtiles
    ,centerY+htiles)
    where   tileDiv s = (s `div` tileSize `div` 2)+1
            wtiles = tileDiv windowWidth
            htiles = tileDiv windowHeight

newGame :: String -> IO Game
newGame mapName = do
    world <- loadWorld mapName
    
    return $ Game
        { time = 0.0
        , npcs  = []
        , world = world
        , rectangle = initRectangle 0 0
        }
    



input event world
    | EventKey (SpecialKey KeyEsc) Down _ _ <- event = error "exit"
    | otherwise = return world 



drawWorld :: World -> Rectangle -> Picture
drawWorld world (x0,y0,x1,y1) = pictures $ do
    (row, y) <- skipTake (y1-y0) y0 $ tiles world
    (tile, x) <- skipTake (x1-x0) x0 row
    return $ drawTile tile x y

    where
        skipTake s t list = (take s . drop t) list `zip` [0..]


-- draw a tile that is already adjusted to the screen
drawTile tile x y = translate nx ny (tilePic tile)
    where
        nx = new x
        ny = new y
        new a = fromIntegral $ a*20


draw :: Game -> IO Picture
draw game = do
    let background = drawWorld (world game) (rectangle game)
    

    return background


update t game = do
    return (game { time = (time game)+t })


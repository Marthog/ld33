import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Game
import Data.Fixed
import Data.List(elemIndex)
import Data.Maybe


import World
import Constants
import Castle
import Game


main = do
    let
        window = InWindow "you are the monster" (100, 100) (windowWidth, windowHeight)

    game <- newGame "test"

    playIO window (greyN 0.5) 60 game draw input update
    




input :: Event -> Game -> IO Game
input event game
    | EventKey (SpecialKey KeyEsc) Down _ _ <- event = error "exit"
    | EventKey (MouseButton LeftButton) Down _ (x,y) <- event = handleClick x y game
    | otherwise = return game 


handleClick :: Float -> Float -> Game -> IO Game
handleClick x y game = do
    let action = getTileAtPosition x y game >>= \(Tile name _) -> Just(print name)
    case action of
        Just a  -> a
        otherwise -> return ()
    return game



getTileAtPosition :: Float -> Float -> Game -> Maybe Tile
getTileAtPosition x y game = do
    getTile (world game) (mk x) (mk y)
    where mk a = round (a / fromIntegral tileSize)



drawWorld :: World -> Rectangle -> Picture
drawWorld world (x0,y0,x1,y1) = pictures $ do
    (row, y) <- skipTake (y1-y0) y0 $ tiles world
    (tile, x) <- skipTake (x1-x0) x0 row
    return $ drawTile tile x y

    where
        skipTake s t list = (take s . drop t) list `zip` [0..]


-- draw a tile that is already adjusted to the screen
drawTile (Tile _ t) x y = translate nx ny t 
    where
        nx = new x
        ny = new y
        new a = fromIntegral $ a*tileSize


draw :: Game -> IO Picture
draw game = do
    let background = drawWorld (world game) (rectangle game)
    
    let castle = drawCastleInfo testCastle

    return $ pictures [background,castle]


update t game = do
    return (game { time = (time game)+t })


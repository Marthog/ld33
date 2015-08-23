import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Game
import Graphics.Gloss.Data.Point

import Data.Fixed
import Data.List(elemIndex)
import Data.Maybe
import Control.Monad

import World
import Constants
import Castle
import Game
import GUI


main = do
    let
        window = InWindow "you are the monster" (100, 100) (windowWidth, windowHeight)

    game <- newGame "test"

    playIO window black 60 game render input update
    


input :: Event -> Game -> IO Game
input event game
    | EventKey (SpecialKey KeyEsc) Down _ _ <- event = error "exit"
    | EventKey (MouseButton LeftButton) Down _ p <- event = clickOn p game (topGui game)
    | otherwise = return game 


handleClickIngame :: ActionHandler Game
handleClickIngame (x,y) game = do
    action <- return $ getTileAtPosition x y game >>= \(Tile name _) -> Just(print name)
    case action of
        Just a  -> a
        otherwise -> return ()
    return game

{-
getTileAtPosition :: Float -> Float -> Game -> Maybe Tile
getTileAtPosition x y game = do
    getTile (world game) (mk x) (mk y)
    where mk a = round (a / fromIntegral tileSize)


-}


drawWorld :: Game -> IRectangle -> Gui Game
drawWorld Game (x0,y0,x1,y1) = 



pictures $ do
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


windowWidthF = fromIntegral windowWidth :: Float
windowHeightF = fromIntegral windowHeight :: Float



data ClickRect = ClickRect Point Point (Game -> IO Game)



gameRect = Rectangle (-windowWidthF/2,-windowHeightF/2) (windowWidthF/2,windowHeightF/2)
ingameRect = Rectangle (-windowWidthF/2,-windowHeightF/2+100) (windowWidthF/2,windowHeightF/2)


background :: Game -> Gui Game
background game = Gui (Button pic handleClickIngame) ingameRect
    where   pic = drawWorld (world game) (rectangle game)


guiBar game = Gui (Group ls)
    (Rectangle(-windowWidthF/2,-windowHeightF/2) (windowWidthF/2,-windowHeightF/2+100))
    where   ls= [ Gui (Static guiBarBottom) $ Rectangle (0,0) (0,0)
                , Gui (Button exitButton (\_ _ -> error "exit"))
                    $ Rectangle (-windowWidthF/2,-50) (-windowWidthF/2+100,50)
                ]


exitButton = pictures [
    rectangleWire 150 100
    , translate (-50) 0 $ scale 0.2 0.2 $ text "Exit"
    ] 


handle :: Point -> Game -> ClickRect -> IO Game
handle cursor game (ClickRect p0 p1 f) = do
    if pointInBox cursor p0 p1
    then f game
    else return game



castle game = Gui (Static $ drawCastleInfo testCastle) $ Rectangle (00,0) (00,0)
topGui game = Gui (Group children) gameRect 
    where   children =
                ($game) `map` [ guiBar
                , background 
                , castle
                ]


guiBarBottom = png "data/gui-down.png"

drawGuiBar :: Game -> Picture
drawGuiBar game =
    let guiBar = guiBarBottom
    in  translate 0.0 (-windowHeightF/2.0+50.0) $ pictures [guiBar]


render :: Game -> IO Picture
render game = do
    --let background = drawWorld (world game) (rectangle game)
    let bgRect = color (greyN 0.5) $!
         rectangleSolid windowWidthF windowHeightF

    return $ pictures [bgRect, draw $ topGui game]
    


update t game = do
    return (game { time = (time game)+t })


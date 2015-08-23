module GUI(
    Rectangle(..)
    ,left,bottom
    ,pointInRect
    ,Gui(..),GuiType(..)
    ,noAction
    ,draw
    ,clickOn
    ,ActionHandler
) where

import Graphics.Gloss
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Control.Monad

type ActionHandler a = Point -> a -> IO a

data Rectangle = Rectangle Point Point
    deriving (Eq, Show)

pointInRect :: Point -> Rectangle -> Bool
pointInRect p (Rectangle p0 p1) = pointInBox p p0 p1

left (Rectangle (x0,_) (x1,_)) = min x0 x1
bottom (Rectangle (_,y0) (_,y1)) = min y0 y1

data GuiType a = Group [Gui a]
    | Static Picture
    | Button Picture (ActionHandler a)


noAction :: ActionHandler a
noAction = \_ -> return

data Gui a = Gui (GuiType a) Rectangle



center (Rectangle (x0,y0) (x1,y1)) = ((x0+x1)/2, (y0+y1)/2)

draw :: Gui a -> Picture
draw (Gui tp rec) = translate x y subPic
    where   subPic = case tp of
                Group ls    -> pictures $ draw `map` ls
                Static pic  -> pic
                Button pic _ -> pic
            (x,y) = center rec


clickOn :: Point -> a -> Gui a -> IO a
clickOn p@(x,y) a (Gui tp rec) =
    if      pointInRect p rec
    then    case tp of
                Group ls    -> foldM (clickOn newP) a ls
                Button _ h  -> h p a
                _           -> return a
    else    return a
    where   newP = (x-cx, y-cy)
            (cx,cy) = center rec


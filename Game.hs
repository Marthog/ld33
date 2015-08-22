module Game
    (GameState
    ,Game(..)
    ,newGame
    ,modifyNpcs
    ,removeNpc
) where

{-# LANGUAGE RecordWildCards #-}

import Control.Monad.State.Lazy
import Data.Unique
import Data.Maybe
import Data.List(deleteBy)
import Data.Function(on)
import Graphics.Gloss

import Constants
import World
import Npc

type GameState = State Game

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


    
data Npc = Npc
    { name          :: !String
    , position      :: !(Float, Float)
    , rotation      :: !Float
    , speed         :: !Float
    , health        :: !Float
    , npcId         :: Unique
    }

instance Eq Npc where
    a == b = npcId a == npcId b


data Target = NpcTarget Npc
    | TargetPos Position

removeNpc :: Npc -> GameState ()
removeNpc npc = modifyNpcList $ deleteBy ((==) `on` npcId) npc


data Animation = Animation
    { frames            :: [Picture]
    , looped            :: !Bool
    , frameTime         :: !Float
    , lastTime          :: !Float
    }




modifyNpcList :: ([Npc] -> [Npc])-> GameState ()
modifyNpcList f = modify $ \s -> s { npcs = f $ npcs s } 



--updateNpcs f = modifyNpcs $ \npcs -> zipWith finalize npcs (f `map` npcs)
--    where   finalize npc | TakeDamage d => if health npc - d then 
        


isNpcInRadius center radius npc = position npc `distanceSq` center <= radiusSq
    where   radiusSq = radius^2


-- placeholder
data Effect = Effect


splashDamage :: Position -> Float -> Float -> Npc -> GameState [Effect]
splashDamage center damage radius self = do
    list <- modifyNpcs makeDamage
    return $ concat list
    where   makeDamage :: Npc -> (Maybe Npc, [Effect])
            makeDamage npc | self==npc          = pass npc
                           | isNpcInRadius center radius npc  = calcDamage npc
                           | otherwise          = pass npc
    
            pass npc   = (Just npc, [])
            calcDamage npc = do
                let newhealth = (health npc)-damage in  if newhealth <= 0.0
                then (Nothing, [Effect])
                else (Just npc, [])



modifyNpcs :: (Npc ->  (Maybe Npc, a)) -> GameState [a]
modifyNpcs f = do
    game <- get
    let (newnpcs, results) = unzip $ f `map` (npcs game)
    let newgame = game {npcs = catMaybes newnpcs}
    put newgame
    return results


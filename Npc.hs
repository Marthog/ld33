{-# LANGUAGE RecordWildCards #-}


module Npc(
    Npc(..)
    ,Animation
    ,playAnimation
) where

import Graphics.Gloss

import World(Position, distanceSq)


data Target = NpcTarget Npc
    | TargetPos Position


data Npc = Npc
    { name          :: !String
    , position      :: !(Float, Float)
    , rotation      :: !Float
    , speed         :: !Float
    , health        :: !Float
    }



data Animation = Animation
    { frames            :: [Picture]
    , looped            :: !Bool
    , frameTime         :: !Float
    , lastTime          :: !Float
    }


playAnimation time anim@Animation{..} = head frames



takeNpcsInRadius center radius = span isInRadius
    where   isInRadius npc = position npc `distanceSq` center <= radiusSq
            radiusSq = radius^2


splashDamage center damage radius self = do
    return 0
    




updateNpc timeDif npc = do
    return 0        
     

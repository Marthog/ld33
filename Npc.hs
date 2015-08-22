module Npc

where


data NpcType = Orc


data ActionType = GoTo
    | Attack Float
    | SplashDamage Float Float

data Action = Action (Int, Int) ActionType


isPathBlocked npcPos pos = False


isNextTo (x0,y0) (x1,y1) = x0-x1<=1 && y0-y1<=1



{-# LANGUAGE RecordWildCards #-}

module Castle (
    Castle(..)
    ,testCastle
    ,drawCastleInfo
)where



import FontRenderer
import System.Random
import Data.Function
import Data.List(sortBy)
import Graphics.Gloss


data Building = Building
    { buildingSize      :: Int
    , workers           :: Int
    , level             :: Int
    , bType             :: BuildingType
    }


data BuildingType = Smith | TrollCave | WolfKennel
    | Tower | Wall



data Castle = Castle
    { gold              :: Int
    , freeWorkers       :: Int
    , food              :: Int
    , weapons           :: Int
    , army          :: Army
    , buildings     :: [Building]
    }

testCastle = Castle{
    gold=50,
freeWorkers=0,food=10,weapons=99,army=[], buildings=[]}


drawCastleInfo :: Castle -> Picture
drawCastleInfo Castle{..} = color white $ scale 0.2 0.2 $ multiline str
    where   str = ["Gold: "++show gold
                , "Food: "++show food
                ] ++ formatArmy army


formatArmy :: Army -> [String]
formatArmy a = (\(m,c) -> show m ++ ": "++show c) `map` a


--drawCastleInfo Castle{..} =

data MonsterType = Soldier | Wolf | Troll | Dragon
    deriving (Show, Eq, Ord)


monsterDamage Soldier   = 1
monsterDamage Wolf      = 10
monsterDamage Troll     = 50
monsterDamage Dragon    = 1000


type Army = [(MonsterType, Int)] 


data Decision = DefenderRetreat | AttackerRetreat | ContinueFight


damage :: Army -> Int
damage = sum . map (\(m,c) -> (monsterDamage m) * c)


fight :: Army -> Army -> IO(Army, Army)
fight a b =
    case decide a b of
        ContinueFight   -> do
            (na, nb) <- battleRound a b
            fight na nb
        _ -> return (a, b)


randomDamage :: Army -> IO Int
randomDamage army = randomRIO (dam-maxDev, dam+maxDev)
    where   dam = damage army
            maxDev = (dam*2) `div` 3


applyDamage :: Int -> Army -> Army
applyDamage dam [] = []
applyDamage dam army =
    let (m,c):ds = reverse $ sortBy (compare `on` fst) army
        mdam = monsterDamage m  :: Int
        minDiff = min c $ dam `mod` mdam
    in (m,c-minDiff):(applyDamage (dam-minDiff*(mdam)) ds)



battleRound :: Army -> Army -> IO (Army, Army)
battleRound a b = do
    da <- randomDamage a
    let a = applyDamage da a
    db <- randomDamage b
    let b = applyDamage db b
    return (a, b)


decide :: Army -> Army -> Decision
decide attacker defender = if da-db> 800 && da>db*5
    then DefenderRetreat
    else if db-da> 500 && db>da*4
    then AttackerRetreat
    else ContinueFight

    where   da = damage attacker
            db = damage defender



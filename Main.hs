module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

--__DATA TYPES__--
type Position = (Float, Float)
type Speed = (Float, Float)

--___WORLD DEFINITIONS__--
data World = World
    { player :: Player
    , enemies :: [Enemy]
    }
initialState :: World
initialState = World
    { player = initialPlayer
    , enemies = [initialEnemy1, initialEnemy2, initialEnemy3, initialEnemy4]
    }
    
--__INPUT DEFINITIONS__--
data Input = Input
    { kJump :: Bool
    , kRight :: Bool
    , kLeft :: Bool
    }
    
--__PLAYER DEFINITIONS__--
data Player = Player
    { getPos :: Position
    , getSpd :: Speed
    , input :: Input
    }
initialPlayer = Player
    { getPos = (0, 0)
    , getSpd = (0, 0)
    , input = Input { kJump = False
                    , kRight = False
                    , kLeft = False}
    }

-- |Update the player
updatePlayer :: Player -> Player
updatePlayer p = playerAction p { getPos = newPos, getSpd = newSpd }
    where
        (newPos, newSpd) = moveEntity (getPos p, getSpd p)
-- |Renders the player
renderPlayer :: Player -> Picture
renderPlayer p = translate x y $ color red $ rectangleSolid tile tile
    where
        (x, y) = getPos p
-- |Changes the player's speed based on user input 
playerAction :: Player -> Player
playerAction p = p { getSpd = (xS', yS') }
    where
        right = kRight $ input p
        left = kLeft $ input p
        (xS, yS) = getSpd p
        xS' = if xor right left
              then (if right then 5 else -5)
              else 0
        yS' = if kJump (input p) && onGround (getPos p)
              then 10
              else yS
              
--__ENEMY DEFINITIONS__--
data Enemy = Enemy
    { getPosE :: Position
    , getSpdE :: Speed
    }
initialEnemy1 = Enemy { getPosE = (0, 100), getSpdE = (4, 0) }
initialEnemy2 = Enemy { getPosE = (20, 110), getSpdE = (-5, 0) }
initialEnemy3 = Enemy { getPosE = (30, 300), getSpdE = (7, 0) }
initialEnemy4 = Enemy { getPosE = (-50, 450), getSpdE = (3, 0) }
-- |Updates the enemy
updateEnemy :: Enemy -> Enemy
updateEnemy e = enemyAction e { getPosE = newPos, getSpdE = newSpd }
    where
        (newPos, newSpd) = moveEntity (getPosE e, getSpdE e)
-- |Renders the enemy
renderEnemy :: Enemy -> Picture
renderEnemy e = translate x y $ color green $ rectangleSolid 32 32
    where (x,y) = getPosE e
-- |Turns the enemy around when it reaches the edge of the screen
enemyAction :: Enemy -> Enemy
enemyAction e = e { getSpdE = spd }
    where
        (x, y) = getPosE e
        (xS, yS) = getSpdE e
        spd = if abs x >= xLimit
              then (negate xS, 20)
              else (xS, yS)
-- |Renders the floor
renderFloor :: Picture
renderFloor = translate 0 (yLimit-tile) $ color black $ rectangleSolid (tile*(fromIntegral width)) tile
        
--__ENTITY FUNCTIONS__--
-- |Returns whether the position is at the bottom of the screen or not
onGround :: Position -> Bool
onGround pos = y == yLimit
    where (x, y) = pos
-- |Checks if the positions given overlap
collision :: Position -> Position -> Bool
collision (x0, y0) (x1, y1) = x0 < x1+tile && x0+tile > x1
                           && y0 < y1+tile && y0+tile > y1
-- |Checks the collision of player with an enemy
died :: World -> Bool
died w = foldl (||) False $ map (\x -> collision playerP x) [getPosE e | e <- (enemies w)]
    where playerP = getPos $ player w

-- |Changes the position and speed of an entity
moveEntity :: (Position, Speed) -> (Position, Speed)
moveEntity (p, s) = ((x',y'), s')
    where
        (x,y) = p
        (xS,yS) = s
        y' = (max (y+yS) yLimit)
        x' = if abs(x+xS) > xLimit
             then xLimit * signum(x)
             else x+xS
        s' = (xS, if onGround p
                  then yS
                  else yS - 1)
    
--__RENDER FUNCTION__--
render :: World -> IO Picture
render world = return $ pictures $ renderPlayer (player world) : renderFloor : applyToAll renderEnemy (enemies world)
        
--__STEP FUNCTION__--
update :: Float -> World -> IO World
update seconds w = return $ if died w
                            then initialState
                            else w { player = updatePlayer (player w)
                                   , enemies = map updateEnemy (enemies w)}

--__HANDLE INPUT__--
handleInput :: Event -> World -> IO World
handleInput (EventKey (Char 'w') Down _ _) w = return $ w { player = (player w) { input = (input (player w)) { kJump = True } } }
handleInput (EventKey (Char 'w') Up _ _) w = return $ w { player = (player w) { input = (input (player w)) { kJump = False } } }
handleInput (EventKey (Char 'a') Down _ _) w = return $ w { player = (player w) { input = (input (player w)) { kLeft = True } } }
handleInput (EventKey (Char 'a') Up _ _) w = return $ w { player = (player w) { input = (input (player w)) { kLeft = False } } }
handleInput (EventKey (Char 'd') Down _ _) w = return $ w { player = (player w) { input = (input (player w)) { kRight = True } } }
handleInput (EventKey (Char 'd') Up _ _) w = return $ w { player = (player w) { input = (input (player w)) { kRight = False } } }
handleInput _ w = return w

--___WINDOW DEFINITIONS___--
width, height, offset :: Int
tile :: Float
tile = 32
width = 24 * round tile
height = 16 * round tile
offset = 100

yLimit = tile - (fromIntegral height) / 2
xLimit = (fromIntegral width) / 2

window :: Display
window = InWindow "Vendo Bolo" (width, height) (offset, offset)

background :: Color
background = white

fps :: Int
fps = 60

runGame = playIO window background fps

main :: IO ()
main = runGame initialState render handleInput update

--__UTIL FUNCTIONS__--
xor :: Bool -> Bool -> Bool
xor True x = not x
xor False x = x

applyToAll :: (a -> b) -> [a] -> [b]
applyToAll f [] = []
applyToAll f (x:xs) = (f x) : (applyToAll f xs)


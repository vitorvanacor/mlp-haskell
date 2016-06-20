module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
--import System.Random

--___WINDOW DEFINITIONS___--
width, height, offset :: Int
tile :: Float
tile = 32
width = 24 * round tile -- |tem como fazer tem função de tile = 32? --
height = 16 * round tile
offset = 100

yLimit = tile - (fromIntegral height) / 2
xLimit = (fromIntegral width) / 2

window :: Display
window = InWindow "MLP" (width, height) (offset, offset)

background :: Color
background = white

--__DATA TYPES__--
type Position = (Float, Float) -- |tem como fazer overload pra (Int, Int)? --
type Speed = (Float, Float)

--___GAME DEFINITIONS__--
data World = World
    { player :: Player
    , enemies :: [Enemy]
    }

possibleBox ::[Position]
possibleBox = []

initialState :: World
initialState = World
    { player = initialPlayer
    , enemies = [initialEnemy1, initialEnemy2]
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
    
updatePlayer :: Player -> Player
updatePlayer p = playerAction p { getPos = newPos, getSpd = newSpd }
    where
        (newPos, newSpd) = moveEntity (getPos p, getSpd p)

renderPlayer :: Player -> Picture
renderPlayer p = translate x y $ color red $ rectangleSolid tile tile
    where
        (x, y) = getPos p
        
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

updateEnemy :: Enemy -> Enemy
updateEnemy e = enemyAction e { getPosE = newPos, getSpdE = newSpd }
    where
        (newPos, newSpd) = moveEntity (getPosE e, getSpdE e)

renderEnemy :: Enemy -> Picture
renderEnemy e = translate x y $ color green $ rectangleSolid 32 32
    where (x,y) = getPosE e
    
enemyAction :: Enemy -> Enemy
enemyAction e = e { getSpdE = spd }
    where
        (x, y) = getPosE e
        (xS, yS) = getSpdE e
        spd = if abs x >= xLimit && (signum x == (signum $ fst $ getSpdE e)) -- Se passou do limite e ta na direção de sair mais (temporario)
              then (negate xS, 20)
              else (xS, yS)
              
--__BOX DEFINITIONS__--
data Box = Box
    { popo :: Position
    }
initialBox = Box {popo = (0,0)}
        
--__FLOOR DEFINITIONS__--              
data Floor = Floor
    { start :: Position
    , end :: Position
    }
floors = [Floor {start = ((-12),(-8)), end = ((-1), (-8))}, Floor {start = ((-12), (-7)), end = ((-12), 7)}]

renderFloor :: Picture
renderFloor = translate 0 (yLimit-tile) $ color black $ rectangleSolid (tile*(fromIntegral width)) tile
        
--__ENTITY FUNCTIONS__--
-- |Returns whether the position is at the bottom of the screen or not
onGround :: Position -> Bool
onGround pos = y == yLimit
    where (x, y) = pos

collision :: Position -> Position -> Bool
collision (x0, y0) (x1, y1) = x0 < x1+tile && x0+tile > x1
                           && y0 < y1+tile && y0+tile > y1
    
died :: World -> Bool
died w = foldl (||) False [collision playerP (getPosE e) | e <- (enemies w)]
    where playerP = getPos $ player w

-- |Changes the position and speed of an entity
moveEntity :: (Position, Speed) -> (Position, Speed)
moveEntity (p, s) = (p', s') -- |teria que passar um world ou lista de floors pra poder testar se bate em alguma parede --
    where
        (x,y) = p
        (xS,yS) = s
        p' = (x+xS, max (y+yS) yLimit) -- | só aumentaria x se não batesse em parede --
        s' = (xS, if onGround p
                  then yS
                  else yS - 1)
    
--__RENDER FUNCTION__--
render :: World -> IO Picture
render world = return $ pictures $ [renderPlayer (player world)] ++ map renderEnemy (enemies world) ++ [renderFloor]
        
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
                    
fps :: Int
fps = 60

main :: IO ()
main = playIO window background fps initialState render handleInput update

xor :: Bool -> Bool -> Bool
xor True x = not x
xor False x = x

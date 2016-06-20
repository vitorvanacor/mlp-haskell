module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import System.Random

--___WINDOW DEFINITIONS___--
width, height, offset :: Int
width = 768 -- |tem como fazer tem função de tile = 32? --
height = 512
offset = 100
botLimit = 32 -(fromIntegral height) / 2
rightLimit = (fromIntegral width) / 2

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
	, box :: Box
	, walls :: [Floor] -- |WTF --
    }

possibleBox ::[Position]
possibleBox = []
	
initialState :: World
initialState = World
    { player = initialPlayer
    , enemies = [initialEnemy1, initialEnemy2]
	, box = initialBox
	, walls = floors -- |WTF--
    }
    
--__INPUT DEFINITIONS__--
data Input = Input
    { kJump :: Bool
    , kRight :: Bool
    , kLeft :: Bool
    }
    
    
--__PLAYER DEFINITIONS__--
data Player = Player
    { position :: Position
    , speed :: Speed
    , input :: Input
    }
initialPlayer = Player
    { position = (0, 0)
    , speed = (0, 0)
    , input = Input { kJump = False
                    , kRight = False
                    , kLeft = False}
    }
    
updatePlayer :: Player -> Player
updatePlayer player = playerAction player { position = newPos, speed = newSpd }
    where
        (newPos, newSpd) = moveEntity (position player, speed player)

renderPlayer :: Player -> Picture
renderPlayer player = translate x y $ color red $ rectangleSolid 32 32
    where
        (x, y) = position player
        
playerAction :: Player -> Player
playerAction p = p { speed = (xS', yS') }
    where
        right = kRight $ input p
        left = kLeft $ input p
        (xS, yS) = speed p
        xS' = if xor right left
              then (if right then 5 else -5)
              else 0
        yS' = if kJump (input p) && onGround (position p)
              then 10
              else yS
--__ENEMY DEFINITIONS__--
data Enemy = Enemy
    { positio :: Position
    , spee :: Speed
    }
initialEnemy1 = Enemy { positio = (0, 100), spee = (4, 0) }
initialEnemy2 = Enemy { positio = (20, 110), spee = (-5, 0) }

updateEnemy :: Enemy -> Enemy
updateEnemy e = enemyAction e { positio = newPos, spee = newSpd }
    where
        (newPos, newSpd) = moveEntity (positio e, spee e)

renderEnemy :: Enemy -> Picture
renderEnemy e = translate x y $ color green $ rectangleSolid 32 32
    where (x,y) = positio e
    
enemyAction :: Enemy -> Enemy
enemyAction e = e { spee = spd }
    where
        (x, y) = positio e
        (xS, yS) = spee e
        spd = if abs x >= rightLimit && (signum x == (signum $ fst $ spee e))
              then (negate xS, yS)
              else (xS, yS)
--__BOX DEFINITIONS__--
data Box = Box
	{ popo :: Position
	}
initialBox = Box {popo = }
			  
--__FLOOR DEFINITIONS__--              
data Floor = Floor
	{ start, end :: Position	
	}
floors = [Floor {start = ((-12),(-8)), end = ((-1), (-8))}, Floor {start = ((-12), (-7)), end = ((-12), 7)}]

renderFloor :: Floor -> Picture
renderFloor f = translate (xs*32) (ys*31) $ color black $ rectangleSolid ((abs (xe - xs))*32) 32 -- |tá errado, n sei onde --
	where
		 (xs, ys) = start f
		 (xe, ye) = end f
			  
--__ENTITY FUNCTIONS__--
-- |Returns whether the position is at the bottom of the screen or not
onGround :: Position -> Bool
onGround pos = y == botLimit
    where (x, y) = pos
	
hitWall :: (Position, Floor) -> Bool
hitWall (pos, f) = x == xs
    where (x, y) = pos
		 (xs, ys) = start f -- |não sei por que dá erro --
		  
hitAnyWall :: (Position, [Floor]) -> Bool -- |parece funcionar --
hitAnyWall (po, lOfFloors) = foldl (||) False [hitWall po x | x <- lOfFloors]

hitEntity ::(Position, Position) -> Bool
hitEntity (p, e) = xP == xE
	where (xP, yP) = p
		  (xE, yE) = e
		  
died :: World -> Bool
died w = foldl (||) False [hitEntity (player w) e | e <- (enemies w)]

-- |Changes the position and speed of an entity
moveEntity :: (Position, Speed) -> (Position, Speed)
moveEntity (p, s) = (p', s') -- |teria que passar um world ou lista de floors pra poder testar se bate em alguma parede --
    where
        (x,y) = p
        (xS,yS) = s
        p' = (x+xS, max (y+yS) botLimit) -- | só aumentaria x se não batesse em parede --
        s' = (xS, if onGround p
                  then yS
                  else yS - 1)

    
--__RENDER FUNCTION__--
render :: World -> IO Picture
render world = return $ pictures $ [renderPlayer (player world)] ++ map renderEnemy (enemies world) ++ map renderFloor floors
        
--__STEP FUNCTION__--
update :: Float -> World -> IO World
update seconds w = return $ if died w
							then initialState
							else World { player = updatePlayer (player w)
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

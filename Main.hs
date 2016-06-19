module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

--___WINDOW DEFINITIONS___--
width, height, offset :: Int
width = 300
height = 600
offset = 100

window :: Display
window = InWindow "MLP" (width, height) (offset, offset)

background :: Color
background = white

--__DATA TYPES__--
type Position = (Float, Float)
type Speed = (Float, Float)

--___GAME DEFINITIONS__--
data World = World
    { player :: Player
    }

initialState :: World
initialState = World
    { player = initialPlayer }
    
--__PLAYER DEFINITIONS__--
data Player = Player
    { position :: Position
    , speed :: Speed
    }
initialPlayer = Player
    { position = (0, 0)
    , speed = (0, 0)
    }
    
updatePlayer :: Player -> Player
updatePlayer player = Player { position = newPos
                             , speed = newSpd }
    where
        newSpd = if onGround player
                 then (0, 0)
                 else applyGravity (speed player)
        newPos = position (applySpeed player)

renderPlayer :: Player -> Picture
renderPlayer player = translate x y $ color red $ rectangleSolid 32 32
    where
        (x, y) = position player

--__ENTITY FUNCTIONS__--
onGround :: Player -> Bool
onGround player = y <= (-(fromIntegral height) / 2) + 64
    where
        (x, y) = position player

applySpeed :: Player -> Player
applySpeed player = Player { position = (x', y')
                           , speed = (xS, yS) }
    where
        (x, y) = position player
        (xS, yS) = speed player
        x' = x + xS
        y' = y + yS

applyGravity :: Speed -> Speed
applyGravity speed = (xS, yS')
    where
       (xS, yS) = speed
       yS' = yS - 2
    
--__RENDER FUNCTION__--
render :: World -> IO Picture
render world = return (renderPlayer (player world))     
        
--__STEP FUNCTION__--
update :: Float -> World -> IO World
update seconds w = return (World {player = updatePlayer (player w)})

--__HANDLE INPUT__--
handleInput :: Event -> World -> IO World
handleInput _ w = return w
                    
fps :: Int
fps = 60

main :: IO ()
main = playIO window background fps initialState render handleInput update

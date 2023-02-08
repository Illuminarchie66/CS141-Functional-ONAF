{-# LANGUAGE MultiWayIf #-}
module OtherUpdates where 

import DataTypes
import Control.Monad.State (State, evalState, get, put, modify, runState, execState)
import AnimationUpdates
import RandomFunc

-- This updates the value of the redDotState (how much it beeps), and the staticIndex (the static caused by the cameras).
-- Uses the modulus value to repeat constantly.
cameraUpdate :: State GameState ()
cameraUpdate  = do
    gs@GameState{..} <- get
    put gs {
    staticIndex = (staticIndex + 1) `mod` 8,
    redDotState = (redDotState + 1) `mod` 96 }

-- This makes a couple checks: Firstly if it is swapping the mode, then it will call the cameraUp animation, and cameraDown if its in camera mode.
-- Then if it doesn't, it renders the right and left doors, whether they are closing/opening, or if they are stationary.
animationUpdate :: State GameState ()
animationUpdate = do
    gs@GameState{..} <- get
    case mode of
        Office | swapping -> put (cameraUp gs)
               | otherwise -> do
                   rightDoor
                   leftDoor
        Cameras | swapping -> put (cameraDown gs)
        _ -> return ()

-- This tells the game that they have made it to 6 AM, meaning the night is over. 
-- If it doesn't reach this, then it will do nothing.

-- This simply increments the frame timer every frame, so we can track total time/total frames.
updateTime :: State GameState ()
updateTime = do
    gs@GameState{..} <- get
    put gs {timer = timer + 1} 

-- This updates the battery by using the original FNAF method. 
-- Every 9.6 seconds (384 frames), 1% will be lost.
-- If a door is shut, then that time is halved; if two doors are shut, then the time is halved again, and so and so forth. 
-- This is calculated using usageFunc.
-- If the interval is not exceeded, we increment the count.
updateInterval :: State GameState () 
updateInterval = do 
    gs@GameState{..} <- get 
    if | intervalCount > (384 / 2**(usageFunc gs)) -> put gs {battery = battery - 1.0, intervalCount = 0}
       | otherwise -> put gs {intervalCount = intervalCount + 1}

-- In the event that nothing is in use, it will divide the time by 1 (2^0 = 1).
usageFunc :: GameState -> Float
usageFunc gs@GameState{..} 
    | x == 0 = 0.0
    | otherwise = fromIntegral x
        where x = length $ filter (==True) [doorLeft, doorRight, mode==Cameras, (lightLeft || lightRight)]

-- This checks the battery every frame, and if it is lower than 0, it knows to start the blackout stage,
-- setting a random countDown until Freddy attacks you.
powerCheck :: State GameState () 
powerCheck = do
    gs@GameState{..} <- get 
    x <- doRandomThing 200 700
    if | battery < 0.0 -> put gs {mode = Blackout, countDown = x}
       | otherwise -> put gs
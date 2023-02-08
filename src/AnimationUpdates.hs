{-# LANGUAGE MultiWayIf #-}
module AnimationUpdates where 

import DataTypes
import Control.Monad.State (State, evalState, get, put, modify, runState, execState)

-- Updates the animation of the cameras by checking the animationState (animState).
-- If animState == 10, then the camera is up, and so the swapping is over.
-- Otherwise the animation frame must increase by one.
cameraUp :: GameState -> GameState
cameraUp gs = if animState gs == 10
    then gs {mode = Cameras, swapping = False}
    else gs {animState = animState gs + 1}

-- Updates the animation of the cameras by checking the animationState (animState).
-- If animState == 0, then the camera is down, and so the swapping is over.
-- Otherwise the animation frame must decrease by one.

-- These two combined allow for the opening of the camera, and the closing of the camera incredibly seamlessly.
cameraDown :: GameState -> GameState
cameraDown gs = if animState gs == 0
    then gs {mode = Office, swapping = False}
    else gs {animState = animState gs - 1}

-- These two functions do the same for either door. 
-- The GameState is updated with the information provided, changing the animationState, and whether it is currently moving.
-- It uses 12 as its base as that is the number of frames there are in its animation. 
-- If it reaches the final or initial moving animation state, it will disable the right/leftDoorMove record.
-- Alternatively it will increase or decrease the animation state until it reaches the initial or final value.
-- Otherwise it will give the same GameState.
rightDoor :: State GameState ()
rightDoor = do
    gs@GameState{..} <- get
    if | rightAnimState == 12 && doorRight -> put gs {rightDoorMove = False}
       | rightAnimState /= 12 && doorRight -> put gs {rightAnimState = rightAnimState + 1}
       | rightAnimState == 0 && not doorRight -> put gs {rightDoorMove = False}
       | rightAnimState /= 0 && not doorRight -> put gs {rightAnimState = rightAnimState -1}
       | otherwise -> put gs

leftDoor :: State GameState ()
leftDoor = do
    gs@GameState{..} <- get
    if | leftAnimState == 12 && doorLeft -> put gs {leftDoorMove = False}
       | leftAnimState /= 12 && doorLeft -> put gs {leftAnimState = leftAnimState + 1}
       | leftAnimState == 0 && not doorLeft -> put gs {leftDoorMove = False}
       | leftAnimState /= 0 && not doorLeft -> put gs {leftAnimState = leftAnimState -1}
       | otherwise -> put gs
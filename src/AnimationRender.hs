{-# LANGUAGE MultiWayIf #-}
module AnimationRender where

import Graphics.Gloss.Interface.Pure.Game
import DataTypes 
import OfficeRender
import GUIRender
import MapRender
import Control.Monad.State (State, evalState, get, put, modify, runState, execState)

-- The process of pulling the camera up.
-- Takes the global images and if in the swapping state, the camera animation frame is returned.
-- Otherwise no picture is returned.
renderCamUp :: [Picture] -> State GameState [Picture]
renderCamUp imgs = do
    gs@GameState{..} <- get
    if | swapping -> return [imgs !! (3 + animState)]
       | otherwise -> return []

-- The process of putting the camera down.
-- Takes the global images and if in the swapping state, the camera animation frame is returned.
-- Otherwise no picture is returned.
-- Additionally, it renders the office behind it if it is pulling down.
-- Otherwise it renders the camera interface.
renderCamDown :: [Picture] -> State GameState [Picture]
renderCamDown imgs = do
    gs@GameState{..} <- get
    if | swapping -> return (officeRender imgs gs ++ renderLeftDoor gs imgs ++ renderRightDoor gs imgs ++ [imgs !! (3 + animState)])
       | otherwise -> return (camRender imgs gs ++ staticRender imgs staticIndex ++ cameraRender imgs gs ++ mapRender imgs currentCam)

-- The process of rendering the blackout office.
-- If you become blackout, then the camera is brought down, determined by animState.
-- Uses a static image of the darkened office.
blackoutRender :: [Picture] -> State GameState [Picture]
blackoutRender imgs = do
    gs@GameState{..} <- get
    if | animState > 0 -> return ([(imgs !! 186), imgs !! (3 + animState)])

    return [translate (position) 0.0 (imgs !! 186)]

-- The processes of rendering the left and right doors. 
-- Translates it to the correct relative position, dependent on the player's position (determined using pixel measurements).
-- Renders the correct door frame. The first/final one in the animation remains the same otherwise.
-- This allows for easy opening and closing.
-- Makes use of the booleans stored in the GameState. 
-- Otherwise, no door will be rendered.
renderLeftDoor :: GameState -> [Picture] -> [Picture]
renderLeftDoor gs@GameState{..} imgs
    | leftDoorMove  = [translate (-460.0 - (-1)*(position ) - 160.0) 0.0 (imgs !! (65 + leftAnimState))]
    | doorLeft = [translate (-460.0 - (-1)*(position) - 160.0) 0.0 (imgs !! 77)]
    | otherwise = []

renderRightDoor :: GameState -> [Picture] -> [Picture]
renderRightDoor gs@GameState{..} imgs
    | rightDoorMove = [translate (455.0 + position + 160.0) 0.0 (imgs !! (78 + rightAnimState))]
    | doorRight = [translate (455.0 + position + 160.0) 0.0 (imgs !! 90)]
    | otherwise = []

-- Renders the jumpscare performed depending on the situation.
-- Currently jumpscares dependent on who is in the office, or the blackout. 
-- When called in the Dying mode, it accesses the animatronic currently held in the Office. 
-- This accesses the details of the jumpscare so it runs correctly. 
-- Bonnie and Chica only jumpscare once the camera is pulled up, as they 'pull' down the camera.
-- Freddy will jumpscare regardless.
-- cameraDown and freddyCam ensures if the camera is up, it is brought down for the jumpscare.
-- Currently non-exhaustive as originally was supposed to include Foxy. 
renderJumpScare :: [Picture] -> State GameState Picture
renderJumpScare imgs = do
    gs@GameState{..} <- get
    let cameraDown = if animState < 0 then [] else [imgs !! (3 + animState)]
        freddyCam = if mode == Cameras then [] else cameraDown
    if | battery < 0.0 -> return (pictures ([imgs !! (188 + deathIndex)]))
       | name inOffice == Bonnie -> return (pictures ([imgs !! (110 + deathIndex `mod` (jumpFrames (inOffice)))] ++ cameraDown))
       | name inOffice == Chica -> return (pictures ([imgs !! (126 + deathIndex `mod` (jumpFrames (inOffice)))] ++ cameraDown))
       | name inOffice == Freddy -> return (pictures ([imgs !! (143 + deathIndex `mod` (jumpFrames (inOffice)))] ++ freddyCam))

-- Renders the static seen after death.
-- Loops through a list of static images.
-- Some say they can see a picture of Freddy in the static. 
renderDeathScreen :: [Picture] -> State GameState Picture
renderDeathScreen imgs = do
    gs@GameState{..} <- get
    return (imgs !! (121 + staticEffect))
module GUIRender where 

import Graphics.Gloss.Interface.Pure.Game
import DataTypes

-- This render function is for the flashing red dot in the corner of the camera screen.
-- Only appears when the redDotState is below 64.
redDotRender :: [Picture] -> GameState -> [Picture]
redDotRender imgs gs
    | redDotState gs > 64 = []
    | otherwise = [translate (-560.0) 280.0 (imgs !! 19)]

-- This draws a path around the screen of a white border for the camera effect.
-- It also calls the redDotRender.
cameraRender :: [Picture] -> GameState -> [Picture]
cameraRender imgs gs = Color white (Line [(-600.0, 320.0), (600.0, 320.0), (600.0, -320.0), (-600.0, -320.0), (-600.0, 320.0)]) : redDotRender imgs gs

-- Calls the battery sprite with different levels of charge depending on the stored float in the GameState. 
-- Works in intervals of 20.0, starting from 100
batteryRender :: [Picture] -> GameState -> Picture
batteryRender imgs gs@GameState{..}
    | battery > 80.0 =(imgs !! 172)
    | battery > 60.0 = (imgs !! 173)
    | battery > 40.0 = (imgs !! 174)
    | battery > 20.0 = (imgs !! 175)
    | battery <= 20.0 = (imgs !! 176)
    | otherwise = (imgs !! 177)

-- Renders the current time in the top right corner of the screen, using the progressing time feature.
-- Shows the times from 12 AM to 6 AM. 
-- Very similar system to the battery.
-- Works with frames rather than seconds (hence multiplying by 40).
timeRender :: [Picture] -> GameState -> Picture
timeRender imgs gs@GameState{..}
    | timer > 445*40 = imgs !! 184
    | timer > 356*40 = imgs !! 183
    | timer > 267*40 = imgs !! 182
    | timer > 178*40 = imgs !! 181
    | timer > 89*40 = imgs !! 180
    | otherwise = imgs !! 179

-- A list of static images used to call in the background. 
-- Seperates them from the main image list, for a more logical structuring (should have done this for more categroies).
staticImages :: [Picture] -> [Picture]
staticImages imgs = [imgs !! 49, imgs !! 49, imgs !! 50, imgs !! 50, imgs !! 51, imgs !! 51, imgs !! 52, imgs !! 52]

-- Renders the static image over the camera screen, creating the creepy haunted effect I am going for.
-- Also looks very cool.
staticRender :: [Picture] -> Int -> [Picture]
staticRender imgs index = [staticImages imgs !! index]

-- Translates and scales the selected time for each frame, using timeRender.
renderTime :: GameState -> [Picture] -> [Picture]
renderTime gs imgs = [translate 580 340 (scale 0.5 0.5 (timeRender imgs gs))]

-- Translates the selected battery for each frame, using batteryRender.
-- (Incredibly creative names).
renderBattery :: GameState -> [Picture] -> [Picture]
renderBattery gs imgs = [translate (-540) (-300) (batteryRender imgs gs)]
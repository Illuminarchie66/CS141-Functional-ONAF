module OfficeRender where

import Graphics.Gloss.Interface.Pure.Game
import DataTypes

-- This is the generic render of the office, this includes the:
-- Backdrop, the left button and the right button. 
-- Each of these are dependent on the position to which the user is looking in the game.
officeRender :: [Picture] -> GameState -> [Picture]
officeRender imgs gs = [translate (position gs) 0.0 (officeBackDropRender imgs gs),
                translate (-580.0 - (-1)*(position gs) - 160.0) (-110.0) (leftButtonRender imgs gs),
                translate (580.0 + position gs + 160.0) (-110.0) (rightButtonRender imgs gs)]

-- This renders the backdrop with either light. 
-- It tackles all cases, whether bonnie/chica is present, and whether the light button is pressed. 
officeBackDropRender :: [Picture] -> GameState -> Picture
officeBackDropRender imgs gs
    | lightLeft gs && room (bonnie gs) /= WestOffice = imgs !! 53
    | lightLeft gs && room (bonnie gs) == WestOffice = imgs !! 55
    | lightRight gs && room (chica gs) /= EastOffice = imgs !! 54
    | lightRight gs && room (chica gs) == EastOffice = imgs !! 56
    | otherwise = head imgs

-- This determines how the left and right buttons are rendered, as they change color when pressed.
-- Since they do this, it means that it has to account for the different cases present.
leftButtonRender :: [Picture] -> GameState -> Picture
leftButtonRender imgs gs
    | doorLeft gs && not (lightLeft gs) = imgs !! 59
    | not (doorLeft gs) && lightLeft gs = imgs !! 60
    | doorLeft gs && lightLeft gs = imgs !! 61
    | otherwise = imgs !! 1

rightButtonRender :: [Picture] -> GameState -> Picture
rightButtonRender imgs gs
    | doorRight gs && not (lightRight gs) = imgs !! 62
    | not (doorRight gs) && lightRight gs = imgs !! 63
    | doorRight gs && lightRight gs = imgs !! 64
    | otherwise = imgs !! 2
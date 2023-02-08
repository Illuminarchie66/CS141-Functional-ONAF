{-# LANGUAGE MultiWayIf #-}
module AnimatronicUpdates where

import DataTypes
import Graph
import Paths 
import RandomFunc
import Control.Monad.State (State, evalState, get, put, modify, runState, execState)
import Debug.Trace
import GHC.IO.Handle (hFlush)
import System.IO (stdout)
import qualified GHC.Base as Freddy
import qualified GHC.Base as Animatronic

-- The effective AI of Bonnie 
-- The Bonnie state gets the current locations that Bonnie could go, and a random value indicating the direction it could go. 
-- The graph accounts for most cases, but specific instances where we want them to only go one way; 
-- The function designates the correct path for it to take. 
-- The frame tracker is reset to 0, so the wait for its next movement begins.
-- Bonnie's direct path effectively follows:
-- The ShowStage -> DiningRoom -> Westhall -> WestCorner -> WestOffice -> Office
-- The backroom and supply closet are additional stops. 
-- Bonnie can only enter the office if the door is open, and there is no other animatronic inside.
-- If either of those conditions aren't met, the position of Bonnie is reset back to the DiningRoom. 
bonnieUpdateLocation :: State GameState ()
bonnieUpdateLocation = do
    gs@GameState{..} <- get
    let locations = getVertex (room bonnie) bonnieGraph
    x <- doRandomThing 0 (length locations -1)
    case room bonnie of
        ShowStage -> put gs {bonnie = bonnie {room = DiningArea, frameTracker = 0}}
        WestCorner -> put gs {bonnie = bonnie {room = WestOffice, frameTracker = 0}}
        WestOffice | not doorLeft && inOffice == None -> put gs {bonnie = bonnie {room = OfficeAnim, frameTracker = 0}, inOffice = bonnie}
        WestOffice -> put gs {bonnie = bonnie {room = DiningArea, frameTracker = 0}}
        otherwise -> put gs {bonnie =  bonnie {room = (locations !! x), frameTracker = 0}}

-- The effective AI of Chica
-- The Chica state gets the current locations that Chica could go, and a random value indicating the direction it could go. 
-- The graph accounts for most cases, but specific instances where we want them to only go one way; 
-- The function designates the correct path for it to take. 
-- The frame tracker is reset to 0, so the wait for its next movement begins.
-- Chica's direct path effectively follows:
-- The ShowStage -> DiningRoom -> Easthall -> EastCorner -> EastOffice -> Office 
-- The kitchen and restrooms are additional potential stops. 
-- Chica can only enter the office if the door is open, and there is no other animatronic inside.
-- If either of those conditions aren't met, the position of Chica is reset back to the DiningRoom. 
chicaUpdateLocation :: State GameState ()
chicaUpdateLocation = do
    gs@GameState{..} <- get
    let locations = getVertex (room chica) chicaGraph
    x <- doRandomThing 0 (length locations -1)
    case room chica of
        ShowStage -> put gs {chica = chica {room = DiningArea, frameTracker = 0}}
        EastOffice | not doorRight && inOffice == None -> put gs {chica = chica {room = OfficeAnim, frameTracker = 0}, inOffice = chica}
        EastOffice -> put gs {chica = chica {room = DiningArea, frameTracker = 0}}
        otherwise -> put gs {chica = chica {room = (locations !! x), frameTracker = 0}}

-- The effective AI of Freddy. 
-- Freddy acts slightly differently, in which every movement oppertunity, it will only move in one direction, with a set path toward the player.
-- Freddy never backtracks.
-- getNextVertex originally was used, but for the sake of debugging, the less eloquent solution of case by case solutions were implemented. 
-- Freddy does not move if the current camera is looking at him. 
-- As opposed to Bonnie and Chica, when encountering a door, he does not turn away, he instead waits there. 
freddyUpdateLocation :: State GameState ()
freddyUpdateLocation = do
    gs@GameState{..} <- get
    case room freddy of
        ShowStage -> put gs {freddy = freddy {room = DiningArea, frameTracker = 0}}
        DiningArea -> put gs {freddy = freddy {room = Restrooms, frameTracker = 0}}
        Restrooms -> put gs {freddy = freddy {room = Kitchen, frameTracker = 0}}
        Kitchen -> put gs {freddy = freddy {room = EastHall, frameTracker = 0}}
        EastHall -> put gs {freddy = freddy {room = EastCorner, frameTracker = 0}}
        EastCorner | not doorRight && inOffice == None-> put gs {freddy = freddy {room = OfficeAnim, frameTracker = 0}, inOffice = freddy}
        EastCorner -> put gs

-- This sequentially calls the actions of Freddy, Bonnie and Chica every frame.
-- Order is irrelevant.
movementActionUpdate :: State GameState ()
movementActionUpdate = do
    gs@GameState{..} <- get
    animatronicAction chica
    animatronicAction bonnie
    animatronicAction freddy

-- This checks that the animatronic is moving on the correct frame.
-- The frame each animatronic moves on is stored within the Animatronic data type, to make it easier to vary. 
-- If the frameTracker ever reaches the correct frame, it will move on to test if it is going to move.
-- Otherwise it increments the frameTracker (each frame).
animatronicAction :: Animatronic -> State GameState ()
animatronicAction x = do
    gs@GameState{..} <- get
    if | frameTracker x > movementTake x -> temp x 
       | otherwise -> case name x of
                        Freddy ->  put gs {freddy = freddy {frameTracker = frameTracker x + 1}} 
                        Bonnie -> put gs {bonnie = bonnie {frameTracker = frameTracker x + 1}}
                        Chica -> put gs  {chica = chica {frameTracker = frameTracker x + 1}}

-- This is a simple function to determine whether Freddy is being looked at. 
-- If he is, then temp2 is called in which the frameTracker is reset. 
-- Otherwise, the movementCheck function will be called on Freddy the same as Bonnie and Chica.
temp :: Animatronic -> State GameState () 
temp x = do
    gs@GameState{..} <- get 
    if | name x == Freddy -> if not (lookingAt freddy gs) then movementCheck x else put gs {freddy = freddy {frameTracker = 0}}
       | otherwise -> movementCheck x
    
-- This calls the effective 'AI' level of each animatronic, and compares it against a random value.
-- Pulling a random value from a seed held in the GameState (to be updated in the doRandomThing).
-- This is compared to the aggression level of the animatronic, which is from 0 to 20. 
-- At 0 the animatronic will take no movement oppertunity.
-- At 20 the animatronic will take every movement oppertunity.
-- If the movement check fails, then the frameTracker is reset.
movementCheck :: Animatronic -> State GameState ()
movementCheck anim = do
    gs@GameState{..} <- get
    -- During the night there is a chance the animatronics stop working, the error has been traced up to here, as the
    -- trace would stop printing past this point. Its incredibly random, with 0 indication as to why. 
    rando <- (doRandomThing 0 20)
    case aggressionLevel anim >= rando of
        True  -> updateLocationCaller anim
        False -> case name anim of
                    Freddy -> put gs {freddy = freddy {frameTracker = 0}}
                    Bonnie -> put gs {bonnie = bonnie {frameTracker = 0}}
                    Chica -> put gs {chica = chica {frameTracker = 0}}

-- This is a simple calling function, so that the function knows what AI to call for each animatronic (dependent on the name).
updateLocationCaller :: Animatronic -> State GameState ()
updateLocationCaller x = do
    case name x of
        Chica -> chicaUpdateLocation
        Bonnie -> bonnieUpdateLocation
        Freddy -> freddyUpdateLocation

-- This is a subsidary function to check whether the current camera is on a certain Animatronic. 
-- Also checks that the mode is on cameras. 
lookingAt :: Animatronic -> GameState -> Bool
lookingAt x gs@GameState{..} = if room x == currentCam && mode == Cameras then True else False
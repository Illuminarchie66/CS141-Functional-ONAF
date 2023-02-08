{-# LANGUAGE MultiWayIf #-}
module DeathUpdates where 

import DataTypes
import Control.Monad.State (State, evalState, get, put, modify, runState, execState)

-- If the player is about to die, then we can tell by an animatronic being in the room. This puts the game in 
-- a traumatic situation, then we can define the conditions for how the animatronic will attack. Currently it is 
-- incredibly basic where if the camera is pulled up, it will jumpscae immediately.
toDie :: State GameState ()
toDie = do
    gs@GameState{..} <- get
    if | inOffice == None -> return ()
       | name inOffice == Bonnie -> if (mode == Cameras) then put gs {mode =  Dying} else put gs
       | name inOffice == Chica -> if mode == Cameras then put gs {mode = Dying} else put gs
       | name inOffice == Freddy -> put gs {mode = Dying}
       | otherwise -> put gs

-- This is updating the death animation of dying, where if the deathIndex is not equal to the number of frames in the 
-- jumpscare animation, then it will increment the deathIndex, and decrement the animationState of the camera being 'pulled down'.
-- When finished it sets the mode to be 'Dead'.
dyingUpdate :: State GameState ()
dyingUpdate = do
    gs@GameState{..} <- get
    case deathIndex == frameMultiplier (inOffice) * jumpFrames (inOffice) of
        True -> put gs {mode = Dead}
        False -> put gs {deathIndex = deathIndex + 1, animState = animState - 1}

-- This is very similar to above, but blackout isn't an animatronic, so we define that the jumpframes of the 
-- mode is 20, as that is what the frames of the second Freddy jumpscare is. 
-- Similarly sets the mode to be 'Dead'.
blackoutDeath :: State GameState ()
blackoutDeath = do 
    gs@GameState{..} <- get 
    case deathIndex == 20 of 
        True -> put gs {mode = Dead}
        False -> put gs {deathIndex = deathIndex + 1}

-- This updates the static index used to update the death screen of the static.
-- This loops over repeatedly using the modulus function. 
deadUpdate :: State GameState ()
deadUpdate = do
    gs@GameState{..} <- get
    put gs {staticEffect = (staticEffect+1) `mod` 5}

-- The blackout update first ensures that the camera is pulled down by decrementing the animationState of the camera (if it is pulled up)
-- Then it checks if the set countdown variable is 0. If not then it decrements countDown;
-- otherwise it has Freddy jumpscare the player once the countdown is over. 
blackoutUpdate :: State GameState ()
blackoutUpdate = do
    gs@GameState{..} <- get
    if | animState > 0 -> put gs {animState = animState - 1}
       | otherwise -> put gs

    if | countDown > 0 -> put gs {countDown = countDown -1}
       | otherwise -> put gs {inOffice = freddy, mode = Dying}
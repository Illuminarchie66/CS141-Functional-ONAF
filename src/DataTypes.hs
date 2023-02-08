module DataTypes where
import System.Random
import qualified GHC.Base as Animatronic's

-- This determines the mode of the game or the state of the player. 
-- This allows for an easy way to check what the game shoukd be doing at a given time.
-- Derives Eq for use, and show for debggging.
data Mode
    = Office
    | Cameras
    | Dying
    | Dead
    | Blackout
    | End
    deriving (Eq, Show)

-- This determines a list of locations that the user can look at.
-- Also determines a list of locations that the animatronic could have access too.
-- Derives Eq to compare cameras to where the user wants to look. 
-- Derives show for debugging.
-- Cams with a name next to them was for some level of organisation. 
data Location
    = ShowStage  --Cam 1A
    | PirateCove    --Cam 1C
    | Restrooms     --Cam 07
    | DiningArea    --Cam 1B
    | WestHall      --Cam 2A
    | WestCorner    --Cam 2B
    | WestOffice
    | EastHall      --Cam 4A
    | EastCorner    --Cam 4B
    | EastOffice
    | SupplyCloset  --Cam 03
    | Backstage     --Cam 05
    | Kitchen        --Cam 06 +
    | OfficeAnim
    deriving (Eq, Show)

-- These names are identifiers for each of the Animatronic's. 
-- We derive Eq as to compare the names when wanting to select specifc animatronic actions.
-- Show was used for debuging.
data Name = Freddy | Bonnie | Chica | Foxy
    deriving (Eq, Show)

-- The animatronic type stores the important data of an animatronic:
-- Name, Room currently in, the number of frames their jumpscare is, the number of times to repeat the jumpscare, 
-- the aggression level used to see how often they move, the frameTracker, to check when the animatronic's try to take an oppertunity,
-- and the frame they try to take that oppertunity.
-- These were designed so that, more data is held by the individual animatronic, and more functions can be generic to an 'animatronic',
-- rather than a string.         
data Animatronic =
    None | Animatronic
    { name :: Name
    , room :: Location
    , jumpFrames :: Int
    , frameMultiplier :: Int
    , aggressionLevel :: Int
    , frameTracker :: Int
    , movementTake :: Int
    }
    deriving (Eq)

-- Simple function used for debugging. 
instance Show Animatronic where
    show x = show (name x)

-- The GameState/World of the program.
-- This is what is called repeatedly, and repeatedly updates, and what the game renders from. 
-- Without this, there is no FNAF. 
-- 
data GameState =
    GameState
    { mode :: Mode                  -- This is the mode of the game, what is occuring at the time.
    , inOffice :: Animatronic       -- This is the animatronic currently in the office. Can be None. 
    , animState :: Int              -- This is the animation state of the camera. Oscillates between 0 and 10 on use.
    , swapping :: Bool              -- This is telling the program if we are swapping between mode Office and mode Cameras. 
    , position :: Float             -- This is the numerical position of where the user's looking with respect to the screen.
    , currentCam :: Location        -- Current location on the cameras you are looking at.
    , staticIndex :: Int            -- Index for determining what static to overlay the cameras.
    , doorLeft :: Bool              -- Whether the left door is down or not.
    , doorRight :: Bool             -- Whether the right door is down or not.
    , lightLeft :: Bool             -- Whether the left light is on or not.
    , lightRight :: Bool            -- Whether the right light is on or not.
    , leftDoorMove :: Bool          -- Whether the left door is moving or not.
    , leftAnimState :: Int          -- The animation state of the door on the left.
    , rightDoorMove :: Bool         -- Whether the right door is moving or not.
    , rightAnimState :: Int         -- The animation state of the door on the right.
    , redDotState :: Int            -- The animation state of the red dot.
    , freddy :: Animatronic         -- The details and store about Freddy Fazbear.
    , bonnie :: Animatronic         -- The details and store about Bonnie the Bunny.
    , chica :: Animatronic          -- The details and store about Chica the Chicken.
    , deathIndex :: Int             -- The index used to keep track of the jumpscare animation frames.
    , rndGen :: StdGen              -- The random seed used to generate pseudo-random numbers.
    , timer :: Int                  -- The in game timer tracking total frames.
    , staticEffect :: Int           -- The index for the static effect for the death screen.
    , battery :: Float              -- The battery's percentage.
    , countDown :: Int              -- Countdown in the blackout until jumpscare
    , intervalCount :: Float        -- How often the battery should deplete by 1.
    }
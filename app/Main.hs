module Main where
import Lib ()
import Graph
import DataTypes
import Paths
import AnimationRender
import MapRender
import GUIRender
import OfficeRender
import Buttons 
import AnimationUpdates 
import AnimatronicUpdates 
import RandomFunc 
import DeathUpdates 
import OtherUpdates

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Data.Maybe (fromJust, isNothing, isJust)
import System.Posix.Internals (get_saved_termios)
import System.IO (localeEncoding)
import System.Random ( getStdRandom, Random(randomR), StdGen, mkStdGen, RandomGen (split), getStdGen )
import Control.Monad.State (State, evalState, get, put, modify, runState, execState)
import Debug.Trace
import GHC.Generics (Generic(to))
import Data.Bits (Bits(xor))
import qualified GHC.Base as Cameras

{- This is the main entry point to your program. -}

-- The window it is fit too is 1280 by 720.
-- Has not been scaled up yet.
window :: Display
window = InWindow "ONAF" (1280, 720) (0, 0)

-- The background color is black, as any risk of potential bacgkground reveals is obscured as black is a similar color.
-- All testing was performed in white.
background :: Color
background = black

-- FPS is set to 40 as the animations work better at a slower rate. 
fps :: Int
fps = 40

-- The render feature has a scenario for each mode of the game. 
-- The most complex are the Office and Cameras.
-- Dying, Dead, Blackout and End are all little more than cutscenes. 
render :: GameState -> [Picture] -> Picture
render gs imgs
    | mode gs == Office = pictures (officeRender imgs gs ++ renderLeftDoor gs imgs ++ renderRightDoor gs imgs  ++ evalState (renderCamUp imgs) gs ++ (renderBattery gs imgs) ++ (renderTime gs imgs))
    | mode gs == Cameras = pictures (evalState (renderCamDown imgs) gs ++ renderBattery gs imgs)
    | mode gs == Dying = evalState (renderJumpScare imgs) gs
    | mode gs == Dead = evalState (renderDeathScreen imgs) gs
    | mode gs == Blackout = pictures (evalState (blackoutRender imgs) gs)
    | mode gs == End = (imgs !! 209)

-- The handle keys function takes in events, such as key presses, and mouse clicks.
-- If space is pressed, then it knows to swap the camera up and down. 
-- If a button is pressed on the screen it is handled by the button managers made.
-- If the left click is not down, the lights cannot be on, so they are set to false. 
-- If nothing is pressed, the game state remains the same. 
handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) gs =
    if swapping gs then gs else gs { swapping = True }
handleKeys (EventMotion (x,y)) gs
    | x > 320.0 && position gs > -160.0 = gs { position = position gs - 10.0}
    | x < -320.0 && position gs < 160.0 = gs { position = position gs + 10.0}
handleKeys (EventKey (MouseButton LeftButton) Down _ (x,y)) gs = handleButtons gs (x,y)
handleKeys (EventKey (MouseButton LeftButton) Up _ _) gs = gs { lightLeft = False, lightRight = False}
handleKeys _ gs = gs

-- This calls execute state, where it calls update'', which is using the state monad.
-- This makes it call out the events in sequence in a way that is useful to us. 
update :: Float -> GameState -> GameState
update dt = execState update''

-- This is where the bulk of the program is initiated, as this covers most of the modes it can be in.
-- If Dead, Blackout or dying there is a specific state to be in which handles all of the computations.
-- Where the real logic happens is in the otherwise section, which tackles the office and cameras.
-- It is update double prime because I did not think one prime was cool enough.
update'' :: State GameState ()
update'' = do
    gs@GameState{..} <- get
    if | mode == Dying -> if (battery < 0.0) then blackoutDeath else dyingUpdate
       | mode == Dead -> deadUpdate
       | mode == Blackout -> blackoutUpdate
       | otherwise -> do
           updateInterval --Updates the battery
           updateTime --Updates the ingame timer (by frame)
           movementActionUpdate --Updates the animatronic's actions
           cameraUpdate --Updates the visuals on the cameras
           animationUpdate --Updates the animations occuring on screen
           powerCheck --Checks if the battery is below what it should be
           toDie --Checks whether the player should be dead.

-- This is all then 'neatly' called within the large main function.
-- This is so large due to the vast amount of assets needed to be implemented.
-- All pngs from loadJuicyPNG are of Maybe type, so the array has fromJust mapped to it.
-- A seed is also set as well, using getStdGen, as to have the randomness take place. 
-- The numbers along the side of the assets are used to refer to the index in the asset list,
-- as working with that many got increasingly confusing.
main :: IO ()
main = do
  seed <- getStdGen
  officeImg <- loadJuicyPNG "assets/Office/39.png"             --0
  leftButtonImg <- loadJuicyPNG "assets/Left/122.png"   --1
  rightButtonImg <- loadJuicyPNG "assets/Right/134.png"
  cameraUp1 <- loadJuicyPNG "assets/cameraAnim/1.png"
  cameraUp2 <- loadJuicyPNG "assets/cameraAnim/2.png"
  cameraUp3 <- loadJuicyPNG "assets/cameraAnim/3.png"   --5
  cameraUp4 <- loadJuicyPNG "assets/cameraAnim/4.png"
  cameraUp5 <- loadJuicyPNG "assets/cameraAnim/5.png"
  cameraUp6 <- loadJuicyPNG "assets/cameraAnim/6.png"
  cameraUp7 <- loadJuicyPNG "assets/cameraAnim/7.png"
  cameraUp8 <- loadJuicyPNG "assets/cameraAnim/8.png"   --10
  cameraUp9 <- loadJuicyPNG "assets/cameraAnim/9.png"
  cameraUp10 <- loadJuicyPNG "assets/cameraAnim/10.png"
  cameraUp11 <- loadJuicyPNG "assets/cameraAnim/11.png"
  static1 <- loadJuicyPNG "assets/static/4.png"
  static2 <- loadJuicyPNG "assets/static/6.png"         --15
  static3 <- loadJuicyPNG "assets/static/8.png"
  static4 <- loadJuicyPNG "assets/static/9.png"
  static5 <- loadJuicyPNG "assets/static/10.png"
  recCirc <- loadJuicyPNG "assets/7.png"
  camsMap0 <- loadJuicyPNG "assets/camsMAP.png"         --20
  camsMap1 <- loadJuicyPNG "assets/Map/1A.png"
  camsMap2 <- loadJuicyPNG "assets/Map/1B.png"
  camsMap3 <- loadJuicyPNG "assets/Map/1C.png"
  camsMap4 <- loadJuicyPNG "assets/Map/2A.png"
  camsMap5 <- loadJuicyPNG "assets/Map/2B.png"
  camsMap6 <- loadJuicyPNG "assets/Map/3.png"
  camsMap7 <- loadJuicyPNG "assets/Map/4A.png"
  camsMap8 <- loadJuicyPNG "assets/Map/4B.png"
  camsMap9 <- loadJuicyPNG "assets/Map/5.png"
  camsMap10 <- loadJuicyPNG "assets/Map/6.png"          --30
  camsMap11 <- loadJuicyPNG "assets/Map/7.png"
  showStage1 <- loadJuicyPNG "assets/cams/Stage/19.png"
  diningRoom1 <- loadJuicyPNG "assets/cams/Dining Room/48.png"
  pirateCove1 <- loadJuicyPNG "assets/cams/Pirate Cove/66.png"
  westHall1 <- loadJuicyPNG "assets/cams/Left Hallway/43.png"
  westCorner1 <- loadJuicyPNG "assets/cams/Left Corner/0.png"
  supplyCloset1 <- loadJuicyPNG "assets/cams/Utility Closet/62.png"
  eastHall1 <- loadJuicyPNG "assets/cams/Right Hallway/67.png"
  eastCorner1 <- loadJuicyPNG "assets/cams/Right Corner/49.png"
  backStage1 <- loadJuicyPNG "assets/cams/Employees only/83.png" --40
  kitchen1 <- loadJuicyPNG "assets/cams/Kitchen/12.png"
  restrooms1 <- loadJuicyPNG "assets/cams/Toilets/41.png"
  staticIm1 <- loadJuicyPNG "assets/static/staticIm1.png"
  staticIm2 <- loadJuicyPNG "assets/static/staticIm2.png"
  staticIm3 <- loadJuicyPNG "assets/static/staticIm3.png" --45
  staticIm4 <- loadJuicyPNG "assets/static/staticIm4.png"
  staticIm5 <- loadJuicyPNG "assets/static/staticIm5.png"
  staticIm6 <- loadJuicyPNG "assets/static/staticIm6.png"
  staticIm7 <- loadJuicyPNG "assets/static/staticIm7.png"
  staticIm8 <- loadJuicyPNG "assets/static/staticIm8.png" --50
  staticIm9 <- loadJuicyPNG "assets/static/staticIm9.png"
  staticIm10 <- loadJuicyPNG "assets/static/staticIm10.png"
  lightLeftNone <- loadJuicyPNG "assets/Office/58.png"
  lightRightNone <- loadJuicyPNG "assets/Office/127.png"
  lightLeftHere <- loadJuicyPNG "assets/Office/225.png" --55
  lightRightHere <- loadJuicyPNG "assets/Office/227.png"
  outOfPower <- loadJuicyPNG "assets/Office/304.png"
  outOfPowerFreddy <- loadJuicyPNG "assets/Office/305.png"
  leftButtonDoor <- loadJuicyPNG "assets/Left/124.png"
  leftButtonLight <- loadJuicyPNG "assets/Left/125.png" -- 60
  leftButtonBoth <- loadJuicyPNG "assets/Left/130.png"
  rightButtonDoor <- loadJuicyPNG "assets/Right/135.png"
  rightButtonLight <- loadJuicyPNG "assets/Right/131.png"
  rightButtonBoth <- loadJuicyPNG "assets/Right/47.png"
  leftDoor0 <- loadJuicyPNG "assets/Left Door/89.png" --65
  leftDoor2 <- loadJuicyPNG "assets/Left Door/91.png"
  leftDoor3 <- loadJuicyPNG "assets/Left Door/92.png"
  leftDoor4 <- loadJuicyPNG "assets/Left Door/93.png"
  leftDoor5 <- loadJuicyPNG "assets/Left Door/94.png"
  leftDoor6 <- loadJuicyPNG "assets/Left Door/95.png" --70
  leftDoor7 <- loadJuicyPNG "assets/Left Door/96.png"
  leftDoor8 <- loadJuicyPNG "assets/Left Door/97.png"
  leftDoor9 <- loadJuicyPNG "assets/Left Door/98.png"
  leftDoor10 <- loadJuicyPNG "assets/Left Door/99.png"
  leftDoor11 <- loadJuicyPNG "assets/Left Door/100.png" --75
  leftDoor12 <- loadJuicyPNG "assets/Left Door/101.png"
  leftDoor13 <- loadJuicyPNG "assets/Left Door/102.png"
  rightDoor0 <- loadJuicyPNG "assets/Right Door/106.png"
  rightDoor1 <- loadJuicyPNG "assets/Right Door/107.png"
  rightDoor2 <- loadJuicyPNG "assets/Right Door/108.png" --80
  rightDoor3 <- loadJuicyPNG "assets/Right Door/109.png"
  rightDoor4 <- loadJuicyPNG "assets/Right Door/110.png"
  rightDoor5 <- loadJuicyPNG "assets/Right Door/111.png"
  rightDoor6 <- loadJuicyPNG "assets/Right Door/112.png"
  rightDoor7 <- loadJuicyPNG "assets/Right Door/113.png" --85
  rightDoor8 <- loadJuicyPNG "assets/Right Door/114.png"
  rightDoor9 <- loadJuicyPNG "assets/Right Door/115.png"
  rightDoor10 <- loadJuicyPNG "assets/Right Door/116.png"
  rightDoor11 <- loadJuicyPNG "assets/Right Door/117.png"
  rightDoor12 <- loadJuicyPNG "assets/Right Door/118.png" --90
  showStage2 <- loadJuicyPNG "assets/cams/Stage/223.png"
  showStage3 <- loadJuicyPNG "assets/cams/Stage/68.png"
  showStage4 <- loadJuicyPNG "assets/cams/Stage/224.png"
  showStage5 <- loadJuicyPNG "assets/cams/Stage/484.png"
  diningRoom2 <- loadJuicyPNG "assets/cams/Dining Room/90.png" --95 : Bonnie 1
  diningRoom3 <- loadJuicyPNG "assets/cams/Dining Room/120.png" --96 : Bonnie 2
  diningRoom4 <- loadJuicyPNG "assets/cams/Dining Room/215.png" --97 : Chica 1
  diningRoom5 <- loadJuicyPNG "assets/cams/Dining Room/222.png" --98 : Chica 2
  diningRoom6 <- loadJuicyPNG "assets/cams/Dining Room/492.png" --99 : Freddy
  westHall2 <- loadJuicyPNG "assets/cams/Left Hallway/206.png"
  westCorner2 <- loadJuicyPNG "assets/cams/Left Corner/188.png"
  backStage2 <- loadJuicyPNG "assets/cams/Employees only/205.png"
  backStage3 <- loadJuicyPNG "assets/cams/Employees only/555.png"
  supplyCloset2 <- loadJuicyPNG "assets/cams/Utility Closet/190.png"
  eastHall2 <- loadJuicyPNG "assets/cams/Right Hallway/221.png" --105
  eastHall3 <- loadJuicyPNG "assets/cams/Right Hallway/226.png"
  eastCorner2 <- loadJuicyPNG "assets/cams/Right Corner/220.png"
  eastCorner3 <- loadJuicyPNG "assets/cams/Right Corner/486.png"
  restrooms2 <- loadJuicyPNG "assets/cams/Toilets/219.png"
  bonnieJumpScare1 <- loadJuicyPNG "assets/Bonnie/291.png" --110
  bonnieJumpScare2 <- loadJuicyPNG "assets/Bonnie/293.png"
  bonnieJumpScare3 <- loadJuicyPNG "assets/Bonnie/294.png"
  bonnieJumpScare4 <- loadJuicyPNG "assets/Bonnie/295.png"
  bonnieJumpScare5 <- loadJuicyPNG "assets/Bonnie/296.png"
  bonnieJumpScare6 <- loadJuicyPNG "assets/Bonnie/297.png" --115
  bonnieJumpScare7 <- loadJuicyPNG "assets/Bonnie/298.png"
  bonnieJumpScare8 <- loadJuicyPNG "assets/Bonnie/299.png"
  bonnieJumpScare9 <- loadJuicyPNG "assets/Bonnie/300.png"
  bonnieJumpScare10 <- loadJuicyPNG "assets/Bonnie/301.png"
  bonnieJumpScare11 <- loadJuicyPNG "assets/Bonnie/303.png" --120
  static6 <- loadJuicyPNG "assets/static/14.png"
  static7 <- loadJuicyPNG "assets/static/15.png"
  static8 <- loadJuicyPNG "assets/static/16.png"
  static9 <- loadJuicyPNG "assets/static/17.png"
  static10 <- loadJuicyPNG "assets/static/18.png" --125
  chicaJumpScare1 <- loadJuicyPNG "assets/Chica/65.png"
  chicaJumpScare2 <- loadJuicyPNG "assets/Chica/69.png"
  chicaJumpScare3 <- loadJuicyPNG "assets/Chica/216.png"
  chicaJumpScare4 <- loadJuicyPNG "assets/Chica/228.png"
  chicaJumpScare5 <- loadJuicyPNG "assets/Chica/229.png"--130
  chicaJumpScare6 <- loadJuicyPNG "assets/Chica/230.png"
  chicaJumpScare7 <- loadJuicyPNG "assets/Chica/231.png"
  chicaJumpScare8 <- loadJuicyPNG "assets/Chica/232.png"
  chicaJumpScare9 <- loadJuicyPNG "assets/Chica/233.png"
  chicaJumpScare10 <- loadJuicyPNG "assets/Chica/234.png"--135
  chicaJumpScare11 <- loadJuicyPNG "assets/Chica/235.png"
  chicaJumpScare12 <- loadJuicyPNG "assets/Chica/236.png"
  chicaJumpScare13 <- loadJuicyPNG "assets/Chica/237.png"
  chicaJumpScare14 <- loadJuicyPNG "assets/Chica/239.png"
  chicaJumpScare15 <- loadJuicyPNG "assets/Chica/279.png"--140
  chicaJumpScare16 <- loadJuicyPNG "assets/Chica/281.png"
  restrooms3 <- loadJuicyPNG "assets/cams/Toilets/494.png"
  freddyJumpScare1 <- loadJuicyPNG "assets/Freddy/Freddy 1/490.png"
  freddyJumpScare2 <- loadJuicyPNG "assets/Freddy/Freddy 1/491.png"
  freddyJumpScare3 <- loadJuicyPNG "assets/Freddy/Freddy 1/493.png" --145
  freddyJumpScare4 <- loadJuicyPNG "assets/Freddy/Freddy 1/495.png"
  freddyJumpScare5 <- loadJuicyPNG "assets/Freddy/Freddy 1/496.png"
  freddyJumpScare6 <- loadJuicyPNG "assets/Freddy/Freddy 1/497.png"
  freddyJumpScare7 <- loadJuicyPNG "assets/Freddy/Freddy 1/498.png"
  freddyJumpScare8 <- loadJuicyPNG "assets/Freddy/Freddy 1/499.png" --150
  freddyJumpScare9 <- loadJuicyPNG "assets/Freddy/Freddy 1/500.png"
  freddyJumpScare10 <- loadJuicyPNG "assets/Freddy/Freddy 1/501.png"
  freddyJumpScare11 <- loadJuicyPNG "assets/Freddy/Freddy 1/502.png"
  freddyJumpScare12 <- loadJuicyPNG "assets/Freddy/Freddy 1/503.png"
  freddyJumpScare13 <- loadJuicyPNG "assets/Freddy/Freddy 1/504.png" --155
  freddyJumpScare14 <- loadJuicyPNG "assets/Freddy/Freddy 1/505.png"
  freddyJumpScare15 <- loadJuicyPNG "assets/Freddy/Freddy 1/506.png"
  freddyJumpScare16 <- loadJuicyPNG "assets/Freddy/Freddy 1/507.png"
  freddyJumpScare17 <- loadJuicyPNG "assets/Freddy/Freddy 1/508.png"
  freddyJumpScare18 <- loadJuicyPNG "assets/Freddy/Freddy 1/509.png" --160
  freddyJumpScare19 <- loadJuicyPNG "assets/Freddy/Freddy 1/510.png"
  freddyJumpScare20 <- loadJuicyPNG "assets/Freddy/Freddy 1/511.png"
  freddyJumpScare21 <- loadJuicyPNG "assets/Freddy/Freddy 1/512.png"
  freddyJumpScare22 <- loadJuicyPNG "assets/Freddy/Freddy 1/513.png"
  freddyJumpScare23 <- loadJuicyPNG "assets/Freddy/Freddy 1/514.png" --165
  freddyJumpScare24 <- loadJuicyPNG "assets/Freddy/Freddy 1/515.png"
  freddyJumpScare25 <- loadJuicyPNG "assets/Freddy/Freddy 1/516.png"
  freddyJumpScare26 <- loadJuicyPNG "assets/Freddy/Freddy 1/517.png"
  freddyJumpScare27 <- loadJuicyPNG "assets/Freddy/Freddy 1/518.png"
  freddyJumpScare28 <- loadJuicyPNG "assets/Freddy/Freddy 1/519.png" --170
  freddyJumpScare29 <- loadJuicyPNG "assets/Freddy/Freddy 1/521.png"
  battery1 <- loadJuicyPNG "assets/battery/bat1.png"
  battery2 <- loadJuicyPNG "assets/battery/bat2.png"
  battery3 <- loadJuicyPNG "assets/battery/bat3.png"
  battery4 <- loadJuicyPNG "assets/battery/bat4.png" --175
  battery5 <- loadJuicyPNG "assets/battery/bat5.png"
  battery6 <- loadJuicyPNG "assets/battery/bat6.png"
  eastHall4 <- loadJuicyPNG "assets/cams/Right Hallway/487.png"
  time12 <- loadJuicyPNG "assets/time/12.png"
  time1 <- loadJuicyPNG "assets/time/1.png" --180
  time2 <- loadJuicyPNG "assets/time/2.png"
  time3 <- loadJuicyPNG "assets/time/3.png"
  time4 <- loadJuicyPNG "assets/time/4.png"
  time5 <- loadJuicyPNG "assets/time/5.png"
  time6 <- loadJuicyPNG "assets/time/6.png" --185
  blackout1 <- loadJuicyPNG "assets/Office/304.png"
  blackout2 <- loadJuicyPNG "assets/Office/305.png"
  blackoutJumpScare1 <- loadJuicyPNG "assets/Freddy/Freddy/F1.png"
  blackoutJumpScare2 <- loadJuicyPNG "assets/Freddy/Freddy/F2.png" 
  blackoutJumpScare3 <- loadJuicyPNG "assets/Freddy/Freddy/F3.png" --190
  blackoutJumpScare4 <- loadJuicyPNG "assets/Freddy/Freddy/F4.png"
  blackoutJumpScare5 <- loadJuicyPNG "assets/Freddy/Freddy/F5.png"
  blackoutJumpScare6 <- loadJuicyPNG "assets/Freddy/Freddy/F6.png"
  blackoutJumpScare7 <- loadJuicyPNG "assets/Freddy/Freddy/F7.png"
  blackoutJumpScare8 <- loadJuicyPNG "assets/Freddy/Freddy/F8.png" --195
  blackoutJumpScare9 <- loadJuicyPNG "assets/Freddy/Freddy/F9.png"
  blackoutJumpScare10 <- loadJuicyPNG "assets/Freddy/Freddy/F10.png"
  blackoutJumpScare11 <- loadJuicyPNG "assets/Freddy/Freddy/F11.png"
  blackoutJumpScare12 <- loadJuicyPNG "assets/Freddy/Freddy/F12.png"
  blackoutJumpScare13 <- loadJuicyPNG "assets/Freddy/Freddy/F13.png" --200
  blackoutJumpScare14 <- loadJuicyPNG "assets/Freddy/Freddy/F14.png"
  blackoutJumpScare15 <- loadJuicyPNG "assets/Freddy/Freddy/F15.png"
  blackoutJumpScare16 <- loadJuicyPNG "assets/Freddy/Freddy/F16.png"
  blackoutJumpScare17 <- loadJuicyPNG "assets/Freddy/Freddy/F17.png"
  blackoutJumpScare18 <- loadJuicyPNG "assets/Freddy/Freddy/F18.png" --205
  blackoutJumpScare19 <- loadJuicyPNG "assets/Freddy/Freddy/F19.png"
  blackoutJumpScare20 <- loadJuicyPNG "assets/Freddy/Freddy/F20.png"
  blackoutJumpScare21 <- loadJuicyPNG "assets/Freddy/Freddy/F21.png"
  endScreen <- loadJuicyPNG "assets/end.png"
  -- This writes the initial state of the game, as well as the settings of the animatronics.
  -- Used for testing and altering values to see specific events and instances.
  -- Encouraged to alter and edit to see specific game instances. 
  -- (Menu was planned but scrapped due to timing).
  let state =
        GameState
          { mode = Office --Start in office, no start screen due to time constraints.
          , inOffice = None
          , animState = 0
          , swapping = False
          , position = 0.0
          , currentCam = ShowStage
          , staticIndex = 0
          , doorLeft = False
          , doorRight = False
          , lightLeft = False
          , lightRight = False
          , leftDoorMove = False
          , leftAnimState = 0
          , rightDoorMove = False
          , rightAnimState = 0
          , redDotState = 0
          -- The Freddy field has all of the unique information about Freddy.
          -- Due to the strength of Freddy, the aggression level is best kept below 10.
          -- movementTake is 121, which is an attempt at moving roughly every 3.025 seconds.
          , freddy = Animatronic {name = Freddy,
                                  room = ShowStage,
                                  jumpFrames = 29,
                                  frameMultiplier = 1,
                                  aggressionLevel = 3,
                                  frameTracker = 0,
                                  movementTake = 121}
          -- The Bonnie field has all of the unique information about Bonnie.
          -- Bonnie's best effects can be seen around a 15-17 aggression level, as that makes them move around a lot.
          -- movementTake is 198, which is an attempt at moving roughly every 4.95 seconds.
          , bonnie = Animatronic {name = Bonnie,
                                  room = ShowStage,
                                  jumpFrames = 11,
                                  frameMultiplier = 3,
                                  aggressionLevel = 17,
                                  frameTracker = 0,
                                  movementTake = 198}
          -- The Chica field has all of the unique information about Chica.
          -- Chica is similar to Bonnie, though is more dangerous on higher levels due to a straighter path. 
          -- movementTake is 202, which is an attempt at moving roughly every 5.05 seconds.
          , chica = Animatronic {name = Chica,
                                 room = ShowStage,
                                 jumpFrames = 16,
                                 frameMultiplier = 3,
                                 aggressionLevel = 17,
                                 frameTracker = 0,
                                 movementTake = 202}
          , deathIndex = 0
          , timer = 533*40
          , rndGen = seed
          , staticEffect = 0
          , battery = 100.0
          , countDown = 0
          , intervalCount = 0
          }
  play
    window
    black
    fps
    state
    (`render` map fromJust [ officeImg,  leftButtonImg,  rightButtonImg,
     cameraUp1,  cameraUp2,  cameraUp3,  cameraUp4,  cameraUp5,  cameraUp6,  cameraUp7,  cameraUp8,  cameraUp9,  cameraUp10,  cameraUp11,
     static1,  static2,  static3,  static4,  static5,
     recCirc,
     camsMap0,  camsMap1,  camsMap2,  camsMap3,  camsMap4,  camsMap5,  camsMap6,  camsMap7,  camsMap8,  camsMap9,  camsMap10,  camsMap11,
     showStage1,
     diningRoom1,
     pirateCove1,
     westHall1,
     westCorner1,
     supplyCloset1,
     eastHall1,
     eastCorner1,
     backStage1,
     kitchen1,
     restrooms1,
     staticIm1,  staticIm2,  staticIm3,  staticIm4,  staticIm5,  staticIm6,  staticIm7,  staticIm8,  staticIm9,  staticIm10,
     lightLeftNone,  lightRightNone,  lightLeftHere,  lightRightHere,  outOfPower,  outOfPowerFreddy,
     leftButtonDoor,  leftButtonLight,  leftButtonBoth,  rightButtonDoor,  rightButtonLight,  rightButtonBoth,
     leftDoor0,  leftDoor2,  leftDoor3,  leftDoor4,  leftDoor5,  leftDoor6,  leftDoor7,  leftDoor8,  leftDoor9,  leftDoor10,  leftDoor11,  leftDoor12,  leftDoor13,
     rightDoor0,  rightDoor1,  rightDoor2,  rightDoor3,  rightDoor4,  rightDoor5,  rightDoor6, rightDoor7,  rightDoor8,  rightDoor9,  rightDoor10,  rightDoor11,  rightDoor12,
     showStage2,  showStage3,  showStage4,  showStage5,
     diningRoom2,  diningRoom3,  diningRoom4,  diningRoom5,  diningRoom6,
     westHall2,
     westCorner2,
     backStage2,  backStage3,
     supplyCloset2,
     eastHall2,  eastHall3,
     eastCorner2,  eastCorner3,
     restrooms2,
     bonnieJumpScare1,  bonnieJumpScare2,  bonnieJumpScare3,  bonnieJumpScare4,  bonnieJumpScare5,  bonnieJumpScare6,  bonnieJumpScare7,  bonnieJumpScare8,  bonnieJumpScare9,  bonnieJumpScare10,  bonnieJumpScare11,
     static6,  static7,  static8,  static9,  static10,
     chicaJumpScare1,  chicaJumpScare2,  chicaJumpScare3,  chicaJumpScare4,  chicaJumpScare5,  chicaJumpScare6,  chicaJumpScare7,  chicaJumpScare8,  chicaJumpScare9,  chicaJumpScare10,  chicaJumpScare11,  chicaJumpScare12, chicaJumpScare13,  chicaJumpScare14,  chicaJumpScare15,  chicaJumpScare16,
     restrooms3,
     freddyJumpScare1, freddyJumpScare2, freddyJumpScare3, freddyJumpScare4, freddyJumpScare5, freddyJumpScare6, freddyJumpScare7, freddyJumpScare8, freddyJumpScare9, freddyJumpScare10, freddyJumpScare11, freddyJumpScare12, freddyJumpScare13, freddyJumpScare14, freddyJumpScare15, freddyJumpScare16, freddyJumpScare17, freddyJumpScare18, freddyJumpScare19, freddyJumpScare20, freddyJumpScare21, freddyJumpScare22, freddyJumpScare23, freddyJumpScare24, freddyJumpScare25, freddyJumpScare26, freddyJumpScare27, freddyJumpScare28, freddyJumpScare29,
     battery1,  battery2,  battery3,  battery4,  battery5,  battery6,
     eastHall4,
     time12,  time1,  time2,  time3,  time4,  time5,  time6,
     blackout1,  blackout2,
     blackoutJumpScare1,  blackoutJumpScare2, blackoutJumpScare3, blackoutJumpScare4, blackoutJumpScare5, blackoutJumpScare6, blackoutJumpScare7, blackoutJumpScare8, blackoutJumpScare9, blackoutJumpScare10, blackoutJumpScare11, blackoutJumpScare12, blackoutJumpScare13, blackoutJumpScare14, blackoutJumpScare15, blackoutJumpScare16, blackoutJumpScare17, blackoutJumpScare18, blackoutJumpScare19, blackoutJumpScare20, blackoutJumpScare21])
    handleKeys
    update

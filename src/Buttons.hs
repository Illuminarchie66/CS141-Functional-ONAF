module Buttons where 
    
import DataTypes

-- These buttons were a large series of pixel measurements.
-- They were calculated based off of the position of the user, and where the images were translated too.
-- Also takes in the coordinates of the mouse. If it is contained within the designated area upon click, it will cause an operation.
-- The buttons in the office depended on the user's position, due to how the buttons were loaded in.
officeButtons :: GameState -> (Float, Float) -> Float -> GameState
officeButtons gs (x,y) position
    | centre1-16.0 < x && x < centre1+22.0 && y < 72.0 + (-110.0) && y > 18.0 + (-110.0) = gs { doorLeft = not (doorLeft gs), leftDoorMove = True }
    | centre1-16.0 < x && x < centre1+22.0 && y < (-7.0) + (-110.0) && y > (-61.0) + (-110.0) = gs { lightLeft = True }
    | centre2-16.0 < x && x < centre2+22.0 && y < 72.0 + (-110.0) && y > 18.0 + (-110.0) = gs { doorRight = not (doorRight gs), rightDoorMove = True }
    | centre2-16.0 < x && x < centre2+22.0 && y < (-7.0) + (-110.0) && y > (-61.0) + (-110.0) = gs { lightRight = True }
    | otherwise = gs
    where
        centre1 = -580.0 - (-1)*position - 160.0
        centre2 = 580.0 + position + 160.0

-- More pixel measurements and more buttons.
-- This very smply changes the current camera to the new one selected.
-- Otherwise it will return nothing. 
mapButtons :: GameState -> (Float, Float) -> GameState
mapButtons gs (x,y)
    | (-86.00)+400.0 < x && x < (-42.0)+400.0 && y < 175.0+(-130.0) && y > 148.0+(-130.0) = gs { currentCam = ShowStage }
    | (-102.0)+400.0 < x && x < (-58.0)+400.0 && y < 114.0+(-130.0) && y > 87.0+(-130.0) = gs { currentCam = DiningArea }
    | (-147.0)+400.0 < x && x < (-103.0)+400.0 && y < 37.0+(-130.0) && y > 10.0+(-130.0) = gs { currentCam = PirateCove }
    | (-89.0)+400.0 < x && x < (-45.0)+400.0 && y < (-88.0)+(-130.0) && y > (-115.0)+(-130.0) = gs { currentCam = WestHall }
    | (-89.0)+400.0 < x && x < (-45.0)+400.0 && y < (-126.0)+(-130.0) && y > (-170)+(-130.0) = gs { currentCam = WestCorner }
    | 2.0+400.0 < x && x < 45.0+400.0 && y < (-88.0)+(-130.0) && y > (-115.0)+(-130.0) = gs { currentCam = EastHall }
    | 2.0+400.0 < x && x < 45.0+400.0 && y < (-126.0)+(-130.0) && y > (-170.0)+(-130.0) = gs { currentCam = EastCorner }
    | (-164.0)+400.0 < x && x < (-120.0)+400.0 && y < (-54.0)+(-130.0) && y > (-81.0)+(-130.0) = gs { currentCam = SupplyCloset }
    | (-197.0)+400.0 < x && x < (-153.0)+400.0 && y < 86.0+(-130.0) && y > 59.0+(-130.0) = gs { currentCam = Backstage }
    | 123.0+400.0 < x && x < 167.0+400.0 && y < (-38.0)+(-130.0) && y > (-65.0)+(-130.0) = gs { currentCam = Kitchen }
    | 122.0+400.0 < x && x < 166.0+400.0 && y < 98.0+(-130.0) && y > 71.0+(-130.0) = gs { currentCam = Restrooms }
    | otherwise = gs

-- This calls each button function within each mode.
-- If it is camera, then the map will generate and appear.
-- If it is the office, then the buttons will need to appear on either side.
-- Otherwise no buttons exist.
handleButtons :: GameState -> (Float, Float) -> GameState
handleButtons gs (x,y)
    | mode gs == Cameras = mapButtons gs (x,y)
    | mode gs == Office = officeButtons gs (x,y) (position gs)
    | otherwise = gs
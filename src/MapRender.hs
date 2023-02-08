module MapRender where 

import Graphics.Gloss.Interface.Pure.Game
import DataTypes 

-- This refers to each state of the map, as when a location is selected, it is highlighted green. 
-- This is done with a new image.
-- Uses pattern recognition to identify the correct image.
mapRender :: [Picture] -> Location -> [Picture]
mapRender imgs ShowStage = [translate 400.0 (-130.0) (imgs !! 21)]
mapRender imgs DiningArea = [translate 400.0 (-130.0) (imgs !! 22)]
mapRender imgs PirateCove = [translate 400.0 (-130.0) (imgs !! 23)]
mapRender imgs WestHall = [translate 400.0 (-130.0) (imgs !! 24)]
mapRender imgs WestCorner = [translate 400.0 (-130.0) (imgs !! 25)]
mapRender imgs SupplyCloset = [translate 400.0 (-130.0) (imgs !! 26)]
mapRender imgs EastHall = [translate 400.0 (-130.0) (imgs !! 27)]
mapRender imgs EastCorner = [translate 400.0 (-130.0) (imgs !! 28)]
mapRender imgs Backstage = [translate 400.0 (-130.0) (imgs !! 29)]
mapRender imgs Kitchen = [translate 400.0 (-130.0) (imgs !! 30)]
mapRender imgs Restrooms = [translate 400.0 (-130.0) (imgs !! 31)]

-- This renders all of the different possibilities with the ShowStage: 
-- All 3 are there; Freddy and Bonnie are there, Freddy and Chica are there, Freddy is there, and nobody is there.
showStageRender :: [Picture] -> GameState -> [Picture]
showStageRender imgs gs@GameState{..}
    | room freddy == ShowStage && room bonnie == ShowStage && room chica == ShowStage = [translate position 0.0 (imgs !! 32)]
    | room freddy == ShowStage && room bonnie /= ShowStage && room chica == ShowStage = [translate position 0.0 (imgs !! 92)]
    | room freddy == ShowStage && room bonnie == ShowStage && room chica /= ShowStage = [translate position 0.0 (imgs !! 91)]
    | room freddy == ShowStage && room bonnie /= ShowStage && room chica /= ShowStage = [translate position 0.0 (imgs !! 93)]
    | otherwise = [translate position 0.0 (imgs !! 94)]

-- The remaining rooms are fairly self explainatory, only thing to note is due to Freddy's mechanic of having to look at him;
-- Freddy is always prioritised to show on screen when two animatronics are in the same location.
-- Foxy is not implemented so Pirate Cove is incredibly simple.
diningAreaRender :: [Picture] -> GameState -> [Picture]
diningAreaRender imgs gs@GameState{..}
    | room freddy == DiningArea = [translate position 0.0 (imgs !! 99)]
    | room bonnie == DiningArea = [translate position 0.0 (imgs !! 96)]
    | room chica == DiningArea = [translate position 0.0 (imgs !! 97)]
    | otherwise = [translate (position) 0.0 (imgs !! 33)]

pirateCoveRender :: [Picture] -> GameState -> [Picture]
pirateCoveRender imgs gs@GameState{..} = [translate position 0.0 (imgs !! 34)]

westHallRender :: [Picture] -> GameState -> [Picture]
westHallRender imgs gs@GameState{..}
    | room bonnie == WestHall = [translate position 0.0 (imgs !! 100)]
    | otherwise = [translate position 0.0 (imgs !! 35)]

westCornerRender :: [Picture] -> GameState -> [Picture]
westCornerRender imgs gs@GameState{..}
    | room bonnie == WestCorner = [translate position 0.0 (imgs !! 101)]
    | otherwise = [translate position 0.0 (imgs !! 36)]

backstageRender :: [Picture] -> GameState -> [Picture]
backstageRender imgs gs@GameState{..}
    | room bonnie == Backstage = [translate position 0.0 (imgs !! 102)]
    | otherwise = [translate position 0.0 (imgs !! 40)]

supplyClosetRender :: [Picture] -> GameState -> [Picture]
supplyClosetRender imgs gs@GameState{..}
    | room bonnie == SupplyCloset = [translate position 0.0 (imgs !! 104)]
    | otherwise = [translate position 0.0 (imgs !! 37)]

eastHallRender :: [Picture] -> GameState -> [Picture]
eastHallRender imgs gs@GameState{..}
    | room freddy == EastHall = [translate position 0.0 (imgs !! 178)]
    | room chica == EastHall = [translate position 0.0 (imgs !! 105)]
    | otherwise = [translate position 0.0 (imgs !! 38)]

eastCornerRender :: [Picture] -> GameState -> [Picture]
eastCornerRender imgs gs@GameState{..}
    | room freddy == EastCorner = [translate position 0.0 (imgs !! 108)]
    | room chica == EastCorner = [translate position 0.0 (imgs !! 107)]
    | otherwise = [translate position 0.0 (imgs !! 39)]

restroomsRender :: [Picture] -> GameState -> [Picture]
restroomsRender imgs gs@GameState{..}
    | room freddy == Restrooms = [translate position 0.0 (imgs !! 142)]
    | room chica == Restrooms = [translate position 0.0 (imgs !! 109)]
    | otherwise = [translate position 0.0 (imgs !! 42)]

-- Calls all of the other functions dependent on the currentCamera being looked at.
-- Current camera is stored so that it opens on the camera left off at.
-- The kitchen is static, so it remains the same image repeatedly. 
camRender :: [Picture] -> GameState -> [Picture]
camRender imgs gs@GameState{..}
    | currentCam == ShowStage = showStageRender imgs gs
    | currentCam == DiningArea = diningAreaRender imgs gs
    | currentCam == PirateCove = pirateCoveRender imgs gs
    | currentCam == WestHall = westHallRender imgs gs
    | currentCam == WestCorner = westCornerRender imgs gs
    | currentCam == SupplyCloset = supplyClosetRender imgs gs
    | currentCam == EastHall = eastHallRender imgs gs
    | currentCam == EastCorner = eastCornerRender imgs gs
    | currentCam == Backstage = backstageRender imgs gs
    | currentCam == Kitchen = [imgs !! 41]
    | currentCam == Restrooms = restroomsRender imgs gs
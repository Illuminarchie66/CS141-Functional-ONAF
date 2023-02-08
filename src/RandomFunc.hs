module RandomFunc where
import DataTypes
import Control.Monad.State (State, evalState, get, put, modify, runState, execState)
import System.Random ( getStdRandom, Random(randomR), StdGen, mkStdGen, RandomGen (split), getStdGen )
import Debug.Trace

-- This uses the state monad to to return the numerical random value, which updating the GameState's seed. 
-- Uses the Random library, specifically randomR, getting a random number in a range.
doRandomThing :: Int -> Int -> State GameState Int
doRandomThing x y = do
        gs@GameState{..} <- get
        let (z, g') = (randomR (x, y) rndGen)
        put gs {rndGen = trace ("Range: " ++ show x ++ ", " ++ show y ++ ": " ++ show z) g'}
        return z
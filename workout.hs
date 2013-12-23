-----------------------------------------------------------
-- WorkoutCycle
-----------------------------------------------------------
-- Cycles have start points and endpoints and minimum
-- increments. 
--
-- Cycle start points should not be < 60% or > 75% 
-- of the endpoint.
--
-- A cycle comprises n steps from start point to end point.
--
-- A step is a multiple of the minimum increment.
-- 
-- If there's no suitable increment available, round the steps.


module Workouts
(
 roundToIncrement
 , wendlerWeek
 , weekOne
 , weekTwo
 , weekThree
 , weekFour
)
where

import System.Environment (getArgs, getProgName)
import Data.List

main = do
    args <- getArgs
    progName <- getProgName
    case args of
        [goal, workouts, weights] -> putStrLn (message goal workouts weights)
        _ -> putStrLn ("Usage: " ++ progName ++ " goal workout weights")
        
message goal workouts weights = show (workoutCycle g wo wg)
    where g  = read goal :: Float
          wo = read workouts :: Float
          wg = map read (words weights)::[Float] 
        

-- subsequences is not in Ubuntu Jaunty's version of GHC, unfortunately.
--subsequences xs = [] : subsequences' xs
--       where subsequences' []     = []
--             subsequences' (x:xs) = [x] : concatMap (\ys -> [ys, x:ys])
--                                                    (subsequences' xs)

steps start end workouts = let step = ((end-start)/(workouts-1)) 
                           in [start, start + step .. end]

threshold workouts goal increment = start >= 0.60 && start <= 0.8 
                         where start = (goal - increment * workouts) / goal

bestIncrement workouts goal increments =  case find (threshold workouts goal) increments of
                                          Just increment -> increment
                                          Nothing -> head increments 

bestStart workouts goal increments = case find (threshold workouts goal) increments of
                                          Just increment -> goal - (workouts * increment)
                                          Nothing -> goal * 0.7

roundToIncrement d n = let raw   = n / d
                           low   = fromInteger(floor (raw) ) * d
                           high  = low + d
                           mid   = d / 2 + low
                       in if n <= mid 
                          then low
                          else high

workoutCycle goal workouts weights = map (roundToIncrement increment) (steps start goal workouts)
                        where increment  = bestIncrement workouts goal increments
                              start      = bestStart  workouts goal increments
                              increments = tail (map (foldl (+) 0) (subsequences weights ))

weekOne = [0.65, 0.75, 0.85]
weekTwo = [0.7, 0.8, 0.9]
weekThree = [0.75, 0.85, 0.95]
weekFour = [0.4, 0.5, 0.6]

wendlerWeek increment oneRM week = map (roundToIncrement increment) $ map (* oneRM) week

wendlerMonth inc oneRM = map (wendlerWeek inc oneRM) [weekOne, weekTwo, weekThree, weekFour]

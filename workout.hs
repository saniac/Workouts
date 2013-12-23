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
 , wendlerMonth
 , main
)
where

import System.Environment (getArgs, getProgName)
import Data.List

main = do
    args <- getArgs
    progName <- getProgName
    case progName of 
        "workout" -> case args of
            [goal, workouts, weights] -> putStrLn (message goal workouts weights)
            _ -> putStrLn ("Usage: " ++ progName ++ " goal workout_count available_weights")
        "wendler" -> case args of
            [increment, oneRM] -> putStrLn (show $ wendlerMonth (read increment :: Float) (read oneRM :: Float))
            _ -> putStrLn ("Usage: " ++ progName ++ " available_increment 1RM")
        
message goal workouts weights = show (workoutCycle g wo wg)
    where g  = read goal :: Float
          wo = read workouts :: Float
          wg = map read (words weights)::[Float] 
        
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

-- TODO There must be a neater way of doing this than going mappity mappity

weekOne = [0.65, 0.75, 0.85]
weekTwo = [0.7, 0.8, 0.9]
weekThree = [0.75, 0.85, 0.95]
weekFour = [0.4, 0.5, 0.6]

wendlerWeek :: Float -> Float -> [Float] -> [Float]
wendlerWeek increment oneRM week = map (roundToIncrement increment) $ map (* oneRM) week

wendlerMonth inc oneRM = map (wendlerWeek inc oneRM) [weekOne, weekTwo, weekThree, weekFour]

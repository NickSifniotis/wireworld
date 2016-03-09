--
-- Uwe R. Zimmer
-- Australia 2015
--

module Measure.Time (

   time_expression -- :: IO e -> IO e -- prints the elapsed time for evaluating the expression e

) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
-- import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- Time is currently measured in overall time which has been found to be more predictable.
-- While CPU time measurements seem more appropriate, Haskell doesn't quite seem to deliver as 
-- advertised on those functions.

time_expression :: IO e -> IO e
time_expression expression = do
   start_time <-  getCurrentTime -- getCPUTime
   value      <- expression
   stop_time  <- getCurrentTime  -- getCPUTime
   let diff = diffUTCTime stop_time start_time -- (fromIntegral (stop_time - start_time)) / (10^12)
   _          <- printf "\tElapsed time: %0.3f sec" (realToFrac diff :: Double) -- (diff :: Double)
   return value
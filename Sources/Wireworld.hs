--
-- Uwe R. Zimmer
-- Australia 2015
--

import Data.Cell (Cell (Head))
import Data.Integer_Subtypes (Nat, from_Pos_to_Int)
import Commandline.Options (args_to_options, Data_Structure (List_2D, Ordered_Lists_2D), Options (world_filename, no_of_tests, model, fps))
import Drawing.Simulation (simulate)
import Load.Wireworld (read_world_from_bmp_file)
import Measure.Time (time_expression)
import System.Environment (getArgs)
import Worlds.As_List_2D          -- (World_model, World_Class (empty_world, prepare_world, transition_world, draw_world, element_occurrence, size))
import Worlds.As_Ordered_Lists_2D -- (World_model, World_Class (empty_world, prepare_world, transition_world, draw_world, element_occurrence, size))
import World_Class (Attributed_World)

iterate_function_n_times :: (a -> a) -> Nat -> a -> a
iterate_function_n_times f n x
   | n > 0     = iterate_function_n_times f (n - 1) (f x)
   | otherwise = x

run_tests :: Attributed_World world 
   -> Nat 
   -> (Attributed_World world -> Nat)
   -> (Cell -> Attributed_World world -> Nat)
   -> IO ()
run_tests a_world runs test_size test_element_occurrence
   | runs > 0 = do 
      putStr "Active heads in the world: "
      time_expression (putStr (show (test_element_occurrence Head a_world))) 
      putStrLn ("\tafter " ++ show runs ++ " transitions on " ++ show (test_size a_world) ++ " cells")
   | otherwise = do
      putStrLn "No measurements are taken - number of tests has been set to zero"

main :: IO ()
main = do
   args <- getArgs
   let options = args_to_options args

   case model options of 

      List_2D -> do
         world <- read_world_from_bmp_file (world_filename options) empty_world prepare_world 
            :: IO (Attributed_World Worlds.As_List_2D.World_model)
  
         let transformed_world = iterate_function_n_times transition_world (no_of_tests options) world
         run_tests transformed_world (no_of_tests options) size element_occurrence

         simulate world draw_world transition_world (from_Pos_to_Int (fps options))

      Ordered_Lists_2D -> do
         world <- read_world_from_bmp_file (world_filename options) empty_world prepare_world 
            :: IO (Attributed_World Worlds.As_Ordered_Lists_2D.World_model)

         let transformed_world = iterate_function_n_times transition_world (no_of_tests options) world
         run_tests transformed_world (no_of_tests options) size element_occurrence

         simulate world draw_world transition_world (from_Pos_to_Int (fps options)) 

--
-- Uwe R. Zimmer
-- Australia 2014
--

module Commandline.Options (
   
   Options (world_filename, no_of_tests, model, fps),
   
   Data_Structure (List_2D, Ordered_Lists_2D),

   default_options,   -- :: Options
   args_to_options    -- :: [String] -> Options
   
) where
   
import Data.Integer_Subtypes (Nat, Pos)
import System.Console.GetOpt (getOpt, ArgOrder (Permute), usageInfo, OptDescr (Option), ArgDescr (OptArg))

data Data_Structure = List_2D | Ordered_Lists_2D
   deriving Eq

data Options = Options {
   world_filename :: String,
   no_of_tests    :: Nat,
   model          :: Data_Structure,
   fps            :: Pos
   }

default_options :: Options
default_options = Options {
   world_filename = "Wireworlds/Playfield.bmp",
   no_of_tests    = 25,
   model          = List_2D,
   fps            = 25
   }

data Flags = World_Filename String | No_Of_Tests Nat | Model Data_Structure | FPS Pos

header :: String
header = "Usage: Wireworld [OPTION...]"

available_options :: [OptDescr Flags]
available_options = [
   Option ['w'] ["world"] (OptArg from_maybe_filename    "<Filename/path>"           ) "a bmp file which contains a wireworld",
   Option ['t'] ["tests"] (OptArg from_maybe_no_of_tests "<Natural number>"          ) "number of test transitions performed",
   Option ['m'] ["model"] (OptArg from_maybe_model       "List_2D | Ordered_Lists_2D") "data structure for the wireworld state",
   Option ['f'] ["fps"  ] (OptArg from_maybe_fps         "<Positive number>"         ) "frames per second for the animation"
   ]

from_maybe_filename :: Maybe String -> Flags
from_maybe_filename maybe_filename = case maybe_filename of
   Nothing       -> World_Filename (world_filename default_options)
   Just filename -> World_Filename filename

from_maybe_no_of_tests :: Maybe String -> Flags
from_maybe_no_of_tests maybe_no_of_tests = case maybe_no_of_tests of
   Nothing                 -> No_Of_Tests (no_of_tests default_options)
   Just no_of_tests_string -> No_Of_Tests (read no_of_tests_string)

from_maybe_model :: Maybe String -> Flags
from_maybe_model maybe_name = case maybe_name of
   Nothing                 -> Model (model default_options)
   Just "List_2D"          -> Model List_2D
   Just "Ordered_Lists_2D" -> Model Ordered_Lists_2D
   _                       -> error "Unknown model given in option '-m'"
   
from_maybe_fps :: Maybe String -> Flags
from_maybe_fps maybe_fps = case maybe_fps of
   Nothing -> FPS (fps default_options)
   Just f  -> FPS (read f)
   
flags_to_options :: [Flags] -> Options
flags_to_options flags = flags_to_options' flags default_options

   where
      flags_to_options' :: [Flags] -> Options -> Options
      flags_to_options' flags' current_options = case flags' of 
         World_Filename name: cs -> flags_to_options' cs (current_options {world_filename = name})
         No_Of_Tests    n   : cs -> flags_to_options' cs (current_options {no_of_tests    = n})
         Model          name: cs -> flags_to_options' cs (current_options {model          = name})
         FPS            f   : cs -> flags_to_options' cs (current_options {fps            = f})
         []                      -> current_options
         
args_to_options :: [String] -> Options
args_to_options args = case getOpt Permute available_options args of
   (flags, [], []) -> flags_to_options flags
   (_, nonOpts, []  ) -> error $ "unrecognized arguments: " ++ unwords nonOpts
   (_, _      , msgs) -> error $ concat msgs ++ usageInfo header available_options
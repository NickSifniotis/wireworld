--
-- Uwe R. Zimmer
-- Australia 2012
--

module World_Class (

   World_Class (
      empty_world,        -- :: Attributed_World world
      prepare_world,      -- :: Loaded_World -> Attributed_World world
      transition_world,   -- :: Attributed_World world -> Attributed_World world
      draw_world,         -- :: Attributed_World world -> Picture
      element_occurrence, -- :: Cell -> Attributed_World world -> Nat
      size                -- ::         Attributed_World world -> Nat
   ),

   World_Dimensions (World_Dim, w_width, w_height),
   Loaded_World (L_World, loaded_dim, loaded_world),
   Attributed_World (A_World, world_dim, world_itself),
   
   fun_world -- :: (world -> world) -> Attributed_World world -> Attributed_World world
      -- Applies a function to the world inside an attributed world - simple helper to make things readable
      
) where

import Data.Cell (Cell)
import Data.Coordinates (Distance)
import Data.Integer_Subtypes (Nat)
import Data.List_2D (List_2D)
import Graphics.Gloss (Picture)

data World_Dimensions       = World_Dim {w_width, w_height :: Distance}
data Loaded_World           = L_World {loaded_dim :: World_Dimensions, loaded_world :: List_2D Cell}
data Attributed_World world = A_World {world_dim  :: World_Dimensions, world_itself :: world}

class World_Class world where  
   empty_world        :: Attributed_World world
   prepare_world      :: Loaded_World -> Attributed_World world
   transition_world   :: Attributed_World world -> Attributed_World world
   draw_world         :: Attributed_World world -> Picture
   element_occurrence :: Cell -> Attributed_World world -> Nat
   size               ::         Attributed_World world -> Nat
   
fun_world :: (world -> world) -> Attributed_World world -> Attributed_World world
fun_world f a_world = A_World {world_dim = world_dim a_world, world_itself = f (world_itself a_world)}
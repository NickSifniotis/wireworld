--
-- Uwe R. Zimmer
-- Australia 2012
--

module Transitions.For_List_2D (
   transition_world -- :: List_2D Cell -> List_2D Cell
) where

--import Data.Cell (Cell)
import Data.Cell (Cell (Head, Tail, Conductor, Empty))
import Data.Coordinates (Coord, Element_w_Coord)
import Data.List_2D
import Data.Integer_Subtypes (Nat)


-- First attempt - O(n) as it goes through the list from start to finish,
-- and O(n) again as it calls functions that are O(n)
-- so this function is O(n squared)
-- yuck!
transition_world :: List_2D Cell -> List_2D Cell
transition_world world = transition_world_recurser world world
   
-- this function exists so it can 'hang on' to the full size world
transition_world_recurser :: List_2D Cell -> List_2D Cell -> List_2D Cell
transition_world_recurser world full_world = case world of
   (e, (x, y)) : es     -> (new_cell_state, (x, y)) : transition_world_recurser es full_world
      where new_cell_state = compute_new_cell_state (e, (x, y)) full_world
   []                   -> []
   
-- this one was shit easy
compute_new_cell_state :: Element_w_Coord Cell -> List_2D Cell -> Cell
compute_new_cell_state (e, (x, y)) world = case e of
   Head                 -> Tail
   Tail                 -> Conductor
   Empty                -> Empty
   Conductor
         | (num_heads == 1 || num_heads == 2)   -> Head
         | otherwise                            -> Conductor
            where num_heads = element_occurrence Head (local_elements (x, y) world)


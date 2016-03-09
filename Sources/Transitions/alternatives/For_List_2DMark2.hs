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
   (e, (x, y)) : es
      | (new_cell_state == Empty)   -> transition_world_recurser es full_world
      | otherwise                   -> (new_cell_state, (x, y)) : transition_world_recurser es full_world
      where new_cell_state = compute_new_cell_state (e, (x, y)) full_world
   []                               -> []
   
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
            
-- construct a list of coordinate/elem pairs that neighbour a Head
-- the elem in this case is a Nat that represents the number of Heads
-- that neighbour this cell
create_neighbours :: List_2D Cell -> List_2D Nat -> List_2D Nat
create_neighbours world working = case world of
   []                      -> []
   (e, (x, y)) : es        -> case e of
      Head                 -> create_neighbours es (insert_neighbour (x, y) working)
      _                    -> create_neighbours es working
      
      
-- insert a head into the neighbours data set
insert_neighbour :: Coord -> List_2D Nat -> List_2D Nat
insert_neighbour (x, y) neighbours = []


-- if the data is in the list, add 1 to it, otherwise, insert it
insert_into :: Coord -> List_2D Nat -> List_2D Nat
insert_into (x, y) neighbours = case data_val of
   Nothing        -> insert_element (1, (x, y)) neighbours
   Just v         -> insert_element (1 + v, (x, y)) (remove (x,y) neighbours)
   where data_val = read_element (x, y) neighbours
         remove :: Coord -> List_2D Nat -> List_2D Nat
         remove (x'', y'') list = case list of
            (e, (x', y')) : ls
               | (x'' == x' && y'' == y')  -> ls
               | otherwise                 -> (e, (x', y')) : remove (x'', y'') ls
            []                             -> []

-- insert_into, mark 2. Might be a bit better than the first version
insert_into' :: Coord -> List_2D Nat -> List_2D Nat
insert_into' (x, y) neighbours = case neighbours of
   (e, (x', y')) : ls
      | (x == x' && y == y')  -> (e + 1, (x, y)) : ls
      | otherwise             -> (e, (x', y')) : insert_into' (x, y) ls
   []                         -> [(1, (x, y))]



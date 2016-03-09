--
-- Uwe R. Zimmer
-- Australia 2012
--

module Transitions.For_Ordered_Lists_2D (
   transition_world -- :: Ordered_Lists_2D Cell -> Ordered_Lists_2D Cell
) where

import Data.Cell (Cell, Cell (Head, Tail, Conductor, Empty))
import Data.Coordinates
import Data.Ordered_Lists_2D


-- Replace this function with something more meaningful:
      
transition_world :: Ordered_Lists_2D Cell -> Ordered_Lists_2D Cell
transition_world world = transition_world_recurser (return_line_triplets world) []
   where
      transition_world_recurser :: [(Sparse_Line Cell, Sparse_Line Cell, Sparse_Line Cell)] -> Ordered_Lists_2D Cell -> Ordered_Lists_2D Cell
      transition_world_recurser triplets new_world = case triplets of
            []                -> new_world
            x: xs             -> transition_world_recurser xs (transition_line x new_world)



return_line_triplets :: Ordered_Lists_2D Cell -> [(Sparse_Line Cell, Sparse_Line Cell, Sparse_Line Cell)]
return_line_triplets world = case world of
   []                         -> []
   [x]                        -> [(Sparse_Line ((y_pos x) - 1) [], x, Sparse_Line ((y_pos x) + 1) [])]
   x: y: rest                 -> (Sparse_Line ((y_pos x) - 1) [], x, y): return_line_triplet_recurser (x: y: rest)
   where return_line_triplet_recurser :: Ordered_Lists_2D Cell -> [(Sparse_Line Cell, Sparse_Line Cell, Sparse_Line Cell)]
         return_line_triplet_recurser smaller_world = case smaller_world of
            []                  -> error "fuckin error m8"
            [_]                 -> error "get stuffed"
            [prev, x]           -> [(prev, x, Sparse_Line ((y_pos x) + 1) [])]
            prev: x: next: rest -> (prev, x, next) : return_line_triplet_recurser (x: next: rest)
            

-- take a list of old entries, transition them, and return a new list of entries
transition_line :: (Sparse_Line Cell, Sparse_Line Cell, Sparse_Line Cell) -> Ordered_Lists_2D Cell -> Ordered_Lists_2D Cell
transition_line (prev, a, next) new_world = case e of
   []          -> new_world
   _           -> (Sparse_Line y (transition_line_recurser e l)): new_world
   where e = entries a
         y = y_pos a
         l = (prev, a, next)
         
         transition_line_recurser :: Placed_Elements Cell -> (Sparse_Line Cell, Sparse_Line Cell, Sparse_Line Cell) -> Placed_Elements Cell
         transition_line_recurser old_elements working_lists = case old_elements of
            []                      -> []
            x: xs                   -> case entry x of
               Empty                ->                                     transition_line_recurser xs working_lists -- place no element, ho ho!
               Head                 -> Placed_Element (x_pos x) Tail     : transition_line_recurser xs working_lists
               Tail                 -> Placed_Element (x_pos x) Conductor: transition_line_recurser xs working_lists
               Conductor            -> case count_heads (x_pos x) working_lists of
                  (1, new_lists)    -> Placed_Element (x_pos x) Head     : transition_line_recurser xs new_lists
                  (2, new_lists)    -> Placed_Element (x_pos x) Head     : transition_line_recurser xs new_lists
                  (_, new_lists)    -> Placed_Element (x_pos x) Conductor: transition_line_recurser xs new_lists
            
            
            
-- this function will count the number of head cells that neighbour the cell in position X
-- it also removes any entries from the list that are < x-1 so the lists do not have to be
-- searched from head to tail over and over. Hopefully this will transform the runtime
-- from n squared to n.
count_heads :: X_Coord -> (Sparse_Line Cell, Sparse_Line Cell, Sparse_Line Cell) -> (Int, (Sparse_Line Cell, Sparse_Line Cell, Sparse_Line Cell))
count_heads x (prev, this, next) = (sub_prev + sub_this + sub_next, (Sparse_Line (y_pos prev) new_prev, Sparse_Line (y_pos this) new_this, Sparse_Line (y_pos next) new_next))
   where new_prev = shrink_to_target x (entries prev)
         new_this = shrink_to_target x (entries this)
         new_next = shrink_to_target x (entries next)
         
         sub_prev = count_proximate_heads x new_prev
         sub_this = count_proximate_heads x new_this
         sub_next = count_proximate_heads x new_next
         
                  
         shrink_to_target :: X_Coord -> Placed_Elements Cell -> Placed_Elements Cell
         shrink_to_target target cells = case cells of
            []                            -> []
            c: cs
               | ((x_pos c) < (target - 1))   -> shrink_to_target target cs
               | otherwise                    -> c: cs


         count_proximate_heads :: X_Coord -> Placed_Elements Cell -> Int
         count_proximate_heads position cells = case cells of
            []                                  -> 0
            c: cs
               | x_pos c > (position + 1)       -> 0
               | entry c == Head                -> 1 + count_proximate_heads position cs
               | otherwise                      ->     count_proximate_heads position cs



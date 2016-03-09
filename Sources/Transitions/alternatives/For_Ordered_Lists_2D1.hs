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
-- I don't even know why I need the y coordinate for this
transition_line :: (Sparse_Line Cell, Sparse_Line Cell, Sparse_Line Cell) -> Ordered_Lists_2D Cell -> Ordered_Lists_2D Cell
transition_line (prev, a, next) new_world = case e of
   []          -> new_world
   _           -> (Sparse_Line y (transition_line_recurser e l)): new_world
   where e = entries a
         y = y_pos a
         l = (prev, a, next)
         
         transition_line_recurser :: Placed_Elements Cell -> (Sparse_Line Cell, Sparse_Line Cell, Sparse_Line Cell) -> Placed_Elements Cell
         transition_line_recurser old_elements l' = case old_elements of
            []          -> []
            x: xs       -> Placed_Element (x_pos x) (transition_cell x l'): transition_line_recurser xs l'




transition_cell :: Placed_Element Cell -> (Sparse_Line Cell, Sparse_Line Cell, Sparse_Line Cell) -> Cell
transition_cell cell l = case entry cell of
   Empty             -> Empty
   Head              -> Tail
   Tail              -> Conductor
   Conductor         -> case count_heads cell l of      -- ha, ha!
      1              -> Head
      2              -> Head
      _              -> Conductor
      
      
count_heads :: Placed_Element Cell -> (Sparse_Line Cell, Sparse_Line Cell, Sparse_Line Cell) -> Int
count_heads cell (a, b, c) = (count_heads_per_line (entries a) target) + (count_heads_per_line (entries b) target) + (count_heads_per_line (entries c) target)
   where target = (x_pos cell) + 1
      
count_heads_per_line :: Placed_Elements Cell -> X_Coord -> Int
count_heads_per_line line target = case line of
   []                            -> 0
   x: xs
      | x_pos x > target         -> 0
      | x_pos x < (target - 2)   -> count_heads_per_line xs target
      | entry (x) == Head        -> 1 + count_heads_per_line xs target
      | otherwise                -> count_heads_per_line xs target


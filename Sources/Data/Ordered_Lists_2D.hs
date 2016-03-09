--
-- Uwe R. Zimmer
-- Australia 2015
--

module Data.Ordered_Lists_2D (

   Ordered_Lists_2D, 
      {- the central data structure of this module: 
      A list of lists where y coordinate are attached to every list at top level (the "lines")
      and every element inside the line lists has an x coordinate attached to it.
      All lists are sorted in ascending order, yet the coordinates do not need to be
      consecutive (or "dense") -}
   
   Sparse_Line    (Sparse_Line, y_pos, entries),
   Placed_Element (Placed_Element, x_pos, entry),
   Placed_Elements,
   
   singleton_world,                     -- :: Element_w_Coord  e -> Ordered_Lists_2D e
   insert_element,                      -- :: Element_w_Coord  e -> Ordered_Lists_2D e -> Ordered_Lists_2D e
   insert_list,                         -- :: List_2D          e -> Ordered_Lists_2D e -> Ordered_Lists_2D e
   combine_Ordered_Lists_2D,            -- :: Ordered_Lists_2D e -> Ordered_Lists_2D e -> Ordered_Lists_2D e
   read_element,                        -- :: Coord     -> Ordered_Lists_2D e -> Maybe e
   element_occurrence,                  -- :: Eq e => e -> Ordered_Lists_2D e -> Nat   

   first_coord,                         -- ::          Ordered_Lists_2D e -> Maybe Coord
   next_coord,                          -- :: Coord -> Ordered_Lists_2D e -> Maybe Coord

   remove_elements_less_than_x,         -- :: X_Coord -> Ordered_Lists_2D e -> Ordered_Lists_2D e
   remove_elements_less_than_y,         -- :: Y_Coord -> Ordered_Lists_2D e -> Ordered_Lists_2D e

   local_lines,                         -- :: Y_Coord -> Ordered_Lists_2D e -> Ordered_Lists_2D e 
      -- +/- 1 y coordinate lines neighbourhood (including the y line itself, if it exists)
   local_elements,                      -- :: Coord -> Ordered_Lists_2D e -> Ordered_Lists_2D e 
   local_elements_list,                 -- :: Coord -> Ordered_Lists_2D e -> [e]
      -- +/- 1 (x, y) coordinates elements neighbourhood - including the element at (x, y) itself, if it exists

   size                                 -- :: Ordered_Lists_2D e -> Nat

) where

import Data.Coordinates (Distance, X_Coord, Y_Coord, Coord, Element_w_Coord)
import Data.Integer_Subtypes (Nat)
import Data.List_2D (List_2D)


data Placed_Element  e = Placed_Element {x_pos :: X_Coord, entry :: e}
type Placed_Elements e = [Placed_Element e]

data Sparse_Line e = Sparse_Line {y_pos :: Y_Coord, entries :: Placed_Elements e}

   
type Ordered_Lists_2D e = [Sparse_Line e]


instance (Show e) => Show (Sparse_Line e) where
   show line = (show (y_pos line)) ++ ": " ++ show (entries line) ++ "\r" ++ "\n"

instance (Show e) => Show (Placed_Element e) where
   show element = (show (x_pos element)) ++ ": " ++ show (entry element)


singleton_world :: Element_w_Coord e -> Ordered_Lists_2D e
singleton_world (cell, (x, y)) = [Sparse_Line {y_pos = y, entries = [Placed_Element {x_pos = x, entry = cell}]}]

insert_element :: Element_w_Coord e -> Ordered_Lists_2D e -> Ordered_Lists_2D e
insert_element (cell, (x, y)) world = case world of 
   l: ls
      | y <  y_pos l -> (Sparse_Line {y_pos = y, entries = insert_cell_to_entries cell x []}): world
      | y == y_pos l -> (Sparse_Line {y_pos = y, entries = insert_cell_to_entries cell x (entries l)}): ls
      | otherwise    -> l: insert_element (cell, (x, y)) ls
   [] -> singleton_world (cell, (x, y))

   where
      insert_cell_to_entries :: e -> X_Coord -> Placed_Elements e -> Placed_Elements e
      insert_cell_to_entries cell' x' entries' = case entries' of 
         c: cs
            | x' <  x_pos c -> Placed_Element {x_pos = x', entry = cell'}: entries'
            | x' == x_pos c -> Placed_Element {x_pos = x', entry = cell'}: cs
            | otherwise     -> c: insert_cell_to_entries cell' x' cs
         [] -> [Placed_Element {x_pos = x', entry = cell'}]

insert_list :: List_2D e -> Ordered_Lists_2D e -> Ordered_Lists_2D e
insert_list list world = case list of
   element_with_coord: cs -> insert_element element_with_coord (insert_list cs world)
   []                     -> world

combine_Ordered_Lists_2D :: Ordered_Lists_2D e -> Ordered_Lists_2D e -> Ordered_Lists_2D e
combine_Ordered_Lists_2D left right = case left of 
   []    -> right
   l: ls -> case entries l of
      c: cs -> combine_Ordered_Lists_2D ((l {entries = cs}): ls) (insert_element ((entry c), (x_pos c, y_pos l)) right)
      []    -> combine_Ordered_Lists_2D ls right
      
read_element :: Coord -> Ordered_Lists_2D e -> Maybe e
read_element (x, y) world = case world of
   l: ls 
      | y <  y_pos l -> Nothing
      | y == y_pos l -> case entries l of
         c: cs
            | x <  x_pos c -> Nothing
            | x == x_pos c -> Just (entry c)
            | otherwise    -> read_element (x, y) [(l {entries = cs})]
         [] -> Nothing
      | otherwise -> read_element (x, y) ls
   [] -> Nothing

element_occurrence :: Eq e => e -> Ordered_Lists_2D e -> Nat
element_occurrence element world = case world of
   l: ls -> contains_element_line element (entries l) + element_occurrence element ls

      where
         contains_element_line :: Eq e => e -> Placed_Elements e -> Nat
         contains_element_line element' cells = case cells of
            c: cs 
               | (entry c) == element' -> 1 + contains_element_line element' cs
               | otherwise             ->     contains_element_line element' cs
            [] -> 0
   [] -> 0

first_coord :: Ordered_Lists_2D e -> Maybe Coord
first_coord world = case world of 
   [] -> Nothing
   l: ls -> case (entries l) of
      []   -> first_coord ls
      c: _ -> Just (x_pos c, y_pos l)

next_coord :: Coord -> Ordered_Lists_2D e -> Maybe Coord
next_coord (x, y) world = case world of
   [] -> Nothing
   l: ls
      | y <  y_pos l -> Nothing
      | y == y_pos l -> next_coord_in_entries x (entries l) l ls
      | otherwise    -> next_coord (x, y) ls
      
   where 
      next_coord_in_entries :: X_Coord -> Placed_Elements e -> Sparse_Line e -> Ordered_Lists_2D e -> Maybe Coord
      next_coord_in_entries x' current_entries current_line rest_world = case current_entries of
         [] -> Nothing
         c: cs
            | x' <  x_pos c -> Nothing
            | x' == x_pos c -> case cs of
               c2: _ -> Just (x_pos c2, y_pos current_line)
               []    -> case rest_world of
                  []    -> Nothing
                  l2: _ -> case (entries l2) of
                     []    -> Nothing
                     c2: _ -> Just (x_pos c2, y_pos l2)
            | otherwise    -> next_coord_in_entries x' cs current_line rest_world

remove_elements_less_than_x :: X_Coord -> Ordered_Lists_2D e -> Ordered_Lists_2D e
remove_elements_less_than_x x world = case world of
   l: ls -> remove_from_line x l: remove_elements_less_than_x x ls
   []    -> []

   where
      remove_from_line :: X_Coord -> Sparse_Line e -> Sparse_Line e
      remove_from_line x' line = line {entries = remove_from_entries x' (entries line)}

         where
            remove_from_entries :: X_Coord -> Placed_Elements e -> Placed_Elements e 
            remove_from_entries x'' cells = case cells of
               c: cs 
                  | x_pos c  < x'' -> remove_from_entries x'' cs
                  | otherwise      -> cells
               [] -> []
            
remove_elements_less_than_y :: Y_Coord -> Ordered_Lists_2D e -> Ordered_Lists_2D e
remove_elements_less_than_y y world = case world of
   [] -> []
   l: ls
      | y_pos l < y -> remove_elements_less_than_y y ls
      | otherwise   -> world

local_lines :: Y_Coord -> Ordered_Lists_2D e -> Ordered_Lists_2D e
local_lines line world = read_neighbouring_lines line 1 world

   where
      read_neighbouring_lines :: Y_Coord -> Distance -> Ordered_Lists_2D e -> Ordered_Lists_2D e
      read_neighbouring_lines y dist world' = case world' of
         l: ls
            | y < (y_pos l) - dist      -> []
            | abs (y - y_pos l) <= dist -> l: read_neighbouring_lines y dist ls
            | otherwise                 ->    read_neighbouring_lines y dist ls
         [] -> []
                  
local_elements :: Coord -> Ordered_Lists_2D e -> Ordered_Lists_2D e
local_elements pos world = read_neighbours pos 1 world

   where
      read_neighbours :: Coord -> Distance -> Ordered_Lists_2D e -> Ordered_Lists_2D e
      read_neighbours (x, y) dist world' = case world' of
         l: ls
            | y < (y_pos l) - dist      -> []
            | abs (y - y_pos l) <= dist -> neighbours_in_line l: (read_neighbours (x, y) dist ls)
            | otherwise                 -> read_neighbours (x, y) dist ls
         [] -> []

         where
            neighbours_in_line :: Sparse_Line e -> Sparse_Line e
            neighbours_in_line line = line {entries = neighbours_in_entries (entries line)}

               where 
                  neighbours_in_entries :: Placed_Elements e -> Placed_Elements e
                  neighbours_in_entries list = case list of
                     c: cs
                        | x < (x_pos c) - dist      -> []
                        | abs (x - x_pos c) <= dist -> c: neighbours_in_entries cs
                        | otherwise                 ->    neighbours_in_entries cs 
                     [] -> [] 

local_elements_list :: Coord -> Ordered_Lists_2D e -> [e]
local_elements_list pos world = read_neighbours_list pos 1 world

   where
      read_neighbours_list :: Coord -> Distance -> Ordered_Lists_2D e -> [e]
      read_neighbours_list (x, y) dist world' = case world' of
         l: ls
            | y < (y_pos l) - dist      -> []
            | abs (y - y_pos l) <= dist -> neighbours_in_entries (entries l) ++ (read_neighbours_list (x, y) dist ls)
            | otherwise                 -> read_neighbours_list (x, y) dist ls
         [] -> []

         where 
            neighbours_in_entries :: Placed_Elements e -> [e]
            neighbours_in_entries list = case list of
               c: cs
                  | x < (x_pos c) - dist      -> []
                  | abs (x - x_pos c) <= dist -> entry c: neighbours_in_entries cs
                  | otherwise                 ->          neighbours_in_entries cs 
               [] -> []

size :: Ordered_Lists_2D e -> Nat
size world = case world of 
   []    -> 0
   l: ls -> no_of_Elements (entries l) + size ls
      where 
         no_of_Elements elements = case elements of 
            []    -> 0
            _: xs -> 1 + no_of_Elements xs
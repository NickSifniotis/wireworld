--
-- Uwe R. Zimmer
-- Australia 2015
--

module Data.List_2D (

   List_2D,
      {- the central data structure of this module: 
      A single list containing elements with their associated coordinates.
      No order on the coordinates is assumed or preseverd -}

   singleton_world,               -- :: Element_w_Coord e -> List_2D e
   insert_element,                -- :: Element_w_Coord e -> List_2D e -> List_2D e
   combine_List_2D,               -- :: List_2D e -> List_2D e -> List_2D e
   read_element,                  -- :: Coord -> List_2D e -> Maybe e
   element_occurrence,            -- :: Eq e => e -> List_2D e -> Nat

   first_coord,                   -- ::          List_2D e -> Maybe Coord
   next_coord,                    -- :: Coord -> List_2D e -> Maybe Coord  

   remove_elements_less_than_x,   -- :: X_Coord -> List_2D e -> List_2D e
   remove_elements_less_than_y,   -- :: Y_Coord -> List_2D e -> List_2D e
   
   local_lines,                   -- :: Y_Coord -> List_2D e -> List_2D e 
      -- +/- 1 y coordinate lines neighbourhood (including the y line itself, if it exists)
   local_elements,                -- :: Coord -> List_2D e -> List_2D e 
   local_elements_list,           -- :: Coord -> List_2D e -> [e]
      -- +/- 1 (x, y) coordinates elements neighbourhood - including the element at (x, y) itself, if it exists

   size                           -- :: List_2D e -> Nat
   
) where

import Data.Coordinates (Distance, Coord, Element_w_Coord, X_Coord, Y_Coord)
import Data.Integer_Subtypes (Nat)

type List_2D e = [Element_w_Coord e]


singleton_world :: Element_w_Coord e -> List_2D e
singleton_world element = [element]

insert_element :: Element_w_Coord e -> List_2D e -> List_2D e
insert_element element world = element: world

combine_List_2D :: List_2D e -> List_2D e -> List_2D e
combine_List_2D list world = list ++ world

-- searches through the unsorted list of coordinates in 'world' until
-- it finds an element at (x, y). Returns Nothing if nothing is found
-- O(n)
read_element :: Coord -> List_2D e -> Maybe e
read_element (x, y) world = case world of
   (element, (x_e, y_e)): cs 
      |    x == x_e 
        && y == y_e -> Just element
      | otherwise   -> read_element (x, y) cs
   [] -> Nothing

-- returns the number of times the element e is found within the list
-- O(n)
element_occurrence :: Eq e => e -> List_2D e -> Nat
element_occurrence element list = case list of
   (local_element, _): cs
      | local_element == element -> 1 + element_occurrence element cs
      | otherwise                ->     element_occurrence element cs
   [] -> 0

-- searches through the unsorted list and returns a Coord that corresponds to
-- the 'top left most' element in the list.
-- O(n)
first_coord :: List_2D e -> Maybe Coord
first_coord world = scan_world_first world Nothing

   where
      scan_world_first :: List_2D e -> Maybe Coord -> Maybe Coord
      scan_world_first world' candidate = case world' of
         (_, (x_e, y_e)): cs -> case candidate of
            Just (x_c, y_c) 
               |     y_e <  y_c 
                 || (y_e == y_c && x_e < x_c) -> scan_world_first cs (Just (x_e, y_e))
               | otherwise                    -> scan_world_first cs candidate
            Nothing -> scan_world_first cs (Just (x_e, y_e))
         [] -> candidate

-- searches through the unsorted list and returns a Coord that corresponds to
-- the 'next' one after the one that is provided
-- again, O(n)
next_coord :: Coord -> List_2D e -> Maybe Coord
next_coord coord world = scan_world_next coord world Nothing

   where
      scan_world_next :: Coord -> List_2D e -> Maybe Coord -> Maybe Coord
      scan_world_next (x, y) world' candidate = case world' of
         (_, (x_e, y_e)): cs -> case candidate of
            Just (x_c, y_c)
               |    (y_e > y   || (y_e == y   && x_e > x  )) 
                 && (y_e < y_c || (y_e == y_c && x_e < x_c)) -> scan_world_next (x, y) cs (Just (x_e, y_e))
               | otherwise                                   -> scan_world_next (x, y) cs candidate
            Nothing
               | y_e > y || (y_e == y && x_e > x) -> scan_world_next (x, y) cs (Just (x_e, y_e))
               | otherwise                        -> scan_world_next (x, y) cs candidate
         [] -> candidate

-- self explanitory. O(n)
remove_elements_less_than_x :: X_Coord -> List_2D e -> List_2D e
remove_elements_less_than_x x world = case world of 
   (element, (x_e, y_e)): cs
      | x_e < x   ->                        remove_elements_less_than_x x cs
      | otherwise -> (element, (x_e, y_e)): remove_elements_less_than_x x cs
   [] -> []

-- self explanitory. O(n)
remove_elements_less_than_y :: Y_Coord -> List_2D e -> List_2D e
remove_elements_less_than_y y world = case world of 
   (element, (x_e, y_e)): cs
      | y_e < y   ->                        remove_elements_less_than_y y cs
      | otherwise -> (element, (x_e, y_e)): remove_elements_less_than_y y cs
   [] -> []


-- returns a List_2D of any cells located on neighbouring lines (by Y coord)
local_lines :: Y_Coord -> List_2D e -> List_2D e
local_lines line world = read_neighbouring_lines line 1 world
   
   where
      read_neighbouring_lines :: Y_Coord -> Distance -> List_2D e -> List_2D e
      read_neighbouring_lines y dist list = case list of
         (element, (x_e, y_e)): cs
            | abs (y_e - y) <= dist -> (element, (x_e, y_e)): read_neighbouring_lines y dist cs
            | otherwise             ->                        read_neighbouring_lines y dist cs
         [] -> []    

-- returns a List_2D of neighbouring cells
-- O(n)
local_elements :: Coord -> List_2D e -> List_2D e
local_elements pos list = read_neighbours pos 1 list

   where
      read_neighbours :: Coord -> Distance -> List_2D e -> List_2D e
      read_neighbours (x, y) dist list' = case list' of
         (element, (x_e, y_e)): cs 
            |    abs (x_e - x) <= dist 
              && abs (y_e - y) <= dist -> (element, (x_e, y_e)): read_neighbours (x, y) dist cs
            | otherwise                ->                        read_neighbours (x, y) dist cs
         [] -> []

-- given Coordinates, returns a list of elements that neighbour them  
-- O fukn (n)    
local_elements_list :: Coord -> List_2D e -> [e]
local_elements_list pos list = read_neighbours_list pos 1 list

   where
      read_neighbours_list :: Coord -> Distance -> List_2D e -> [e]
      read_neighbours_list (x, y) dist list' = case list' of
         (element, (x_e, y_e)): cs 
            |    abs (x_e - x) <= dist 
              && abs (y_e - y) <= dist -> element: read_neighbours_list (x, y) dist cs
            | otherwise                ->          read_neighbours_list (x, y) dist cs
         [] -> []

-- O(n) returns the number of elements in the given list
size :: List_2D e -> Nat
size list = case list of
   []    -> 0
   _: xs -> 1 + size xs

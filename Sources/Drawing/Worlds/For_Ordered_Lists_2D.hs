--
-- Uwe R. Zimmer
-- Australia 2015
--

module Drawing.Worlds.For_Ordered_Lists_2D ( 
   draw_world -- :: Ordered_Lists_2D Cell -> Float -> Picture
) where

import Data.Cell (Cell)
import Data.Coordinates (Element_w_Coord, Y_Coord)
import Data.Ordered_Lists_2D (Ordered_Lists_2D, Sparse_Line, y_pos, entries, Placed_Elements, x_pos, entry)
import Drawing.Cell (draw_cell)
import Graphics.Gloss (Picture (Pictures))

draw_world :: Ordered_Lists_2D Cell -> Float -> Picture
draw_world world cell_size = Pictures (map_Ordered_Lists_2D_w_context_to_list draw_cell cell_size world)

   where
      map_Ordered_Lists_2D_w_context_to_list :: (Element_w_Coord e -> c -> b) -> c -> Ordered_Lists_2D e -> [b]
      map_Ordered_Lists_2D_w_context_to_list f context world' =  case world' of
         l: ls -> map_line f context l ++ map_Ordered_Lists_2D_w_context_to_list f context ls
         []    -> []

         where
            map_line :: (Element_w_Coord e -> c -> b) -> c -> Sparse_Line e -> [b]
            map_line f' context' line = map_elements f' context' (y_pos line) (entries line)

               where
                  map_elements :: (Element_w_Coord e -> c -> b) -> c -> Y_Coord -> Placed_Elements e -> [b]
                  map_elements f'' context'' y elements = case elements of
                     c: cs -> f'' ((entry c), ((x_pos c), y)) context'' : map_elements f'' context'' y cs
                     []    -> []

      
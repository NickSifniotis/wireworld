--
-- Uwe R. Zimmer
-- Australia 2012
--

module Drawing.Worlds.For_List_2D ( 
   draw_world -- :: List_2D Cell -> Float -> Picture
) where

import Data.Cell (Cell)
import Data.Coordinates (Element_w_Coord)
import Data.List_2D (List_2D)
import Drawing.Cell (draw_cell)
import Graphics.Gloss (Picture (Pictures))

draw_world :: List_2D Cell -> Float -> Picture
draw_world world cell_size = Pictures (map_List_2D_w_context_to_list draw_cell cell_size world)

   where
      map_List_2D_w_context_to_list :: (Element_w_Coord e -> c -> b) -> c -> List_2D e -> [b]
      map_List_2D_w_context_to_list f context list = case list of
         c: cs -> f c context: map_List_2D_w_context_to_list f context cs
         []    -> []
      

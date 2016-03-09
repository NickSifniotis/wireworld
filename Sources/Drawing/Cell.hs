--
-- Uwe R. Zimmer
-- Australia 2012
--

module Drawing.Cell ( 

   draw_cell -- :: (Cell, Coord) -> Float -> Picture

) where

import Data.Cell (Cell)
import Data.Coordinates (Coord)
import Drawing.Cell_To_Colour (cell_to_colour)
import Graphics.Gloss (Picture, rectangleSolid, translate, color)

draw_cell :: (Cell, Coord) -> Float -> Picture
draw_cell (cell, (x, y)) size = 
    translate x' y' (color (cell_to_colour cell) (rectangleSolid (size - 1) (size - 1)))
        where x' = size * fromIntegral x
              y' = size * fromIntegral y
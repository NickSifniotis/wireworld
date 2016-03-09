--
-- Uwe R. Zimmer
-- Australia 2012
--

module Load.Pixel_To_Cell (

   Pixel (Pixel, red, green, blue, alpha),

   pixel_to_cell -- :: Pixel -> Cell

) where

import Data.Cell (Cell (Head, Tail, Conductor, Empty))
import Data.Word (Word8)

data Pixel = Pixel {red, green, blue, alpha :: Word8}

pixel_to_cell :: Pixel -> Cell
pixel_to_cell Pixel {red = r, green = g, blue = b, alpha = _}
   | r >= 128 && g >= 128 && b <  128 = Conductor -- somewhat yellow
   | r >= 128 && g <  128 && b <  128 = Tail      -- somewhat red
   | r <  128 && g <  128 && b >= 128 = Head      -- somewhat blue
   | otherwise                        = Empty


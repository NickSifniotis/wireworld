--
-- Uwe R. Zimmer
-- Australia 2015
--

module Drawing.Constants ( 

   Window_Size (x_dim, y_dim),
   Window_Pos  (x_pos, y_pos),
   
   window_size,
   window_pos,
   
   cell_size

) where

import World_Class (World_Dimensions (w_width, w_height))

data Window_Size = Window_Size {x_dim, y_dim :: Int}
data Window_Pos  = Window_Pos  {x_pos, y_pos :: Int}

window_size :: Window_Size
window_pos  :: Window_Pos

window_size = Window_Size {x_dim = 800, y_dim = 800} 
window_pos  = Window_Pos  {x_pos =  10, y_pos =  10} 

cell_size :: World_Dimensions -> Float
cell_size dim = (1.0 - border_percentage) * min (x_window_size / f_width) (y_window_size / f_height)
   
   where
      (x_window_size, y_window_size) = (fromIntegral (x_dim window_size), fromIntegral (y_dim window_size))
      (f_width, f_height)            = (fromInteger (w_width dim)       , fromInteger (w_height dim))
      border_percentage = 0.05 -- meaning 5% of the tighter dimension will be used for border

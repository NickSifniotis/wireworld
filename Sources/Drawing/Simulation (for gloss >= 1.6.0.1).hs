--
-- Uwe R. Zimmer
-- Australia 2013
--

-- Version for gloss 1.6.0.1 (or newer).

module Drawing.Simulation (

   simulate -- :: Attributed_World world                                                      -- starting state
            --    -> (Attributed_World world -> Picture)                                      -- drawing function
            --    -> (ViewPort -> Float -> Attributed_World world -> Attributed_World world)  -- transition function
            --    -> Int                                                                      -- frames per second
            --    -> IO ()

) where

import Drawing.Constants (Window_Size (x_dim, y_dim), Window_Pos  (x_pos, y_pos), window_size, window_pos)
import Graphics.Gloss (Display (InWindow), black, Picture)
import qualified Graphics.Gloss (simulate)
import World_Class (Attributed_World)

simulate :: Attributed_World world 
   -> (Attributed_World world -> Picture) 
   -> (Attributed_World world -> Attributed_World world)
   -> Int
   -> IO ()
simulate a_world draw transfer fps = do
   Graphics.Gloss.simulate
      (InWindow                                 -- In a window
         "Wireworld"                            -- Window title
         (x_dim window_size, y_dim window_size) -- Window size
         (x_pos window_pos , y_pos window_pos)) -- Window position
      black                                     -- Background colour
      fps                                       -- Transitions per second
      a_world                                   -- Starting state
      draw                                      -- Function to draw world
      (\_ _ -> transfer)                        -- Step world one state
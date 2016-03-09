--
-- Uwe R. Zimmer
-- Australia 2012
--
-- based on a module by:
--
-- By Ludvik 'Probie' Galois
-- Modified 10 Feb 2012

module Load.Wireworld ( 

   read_world_from_bmp_file -- :: String                                      -- Filename
                            --    -> Attributed_World world                   -- Empty world
                            --    -> (Loaded_World -> Attributed_World world) -- prepare function
                            --    -> IO (Attributed_World world)              -- returns the loaded and prepared world

) where

import Codec.BMP (readBMP, BMP, bmpDimensions, unpackBMPToRGBA32)
import Data.ByteString (unpack)
import Data.Cell (Cell (Empty))
import Data.Word (Word8)
import Load.Pixel_To_Cell (Pixel (Pixel, red, green, blue, alpha), pixel_to_cell)
import System.IO (stderr, hPutStrLn)
import World_Class ( Attributed_World, 
                     Loaded_World (L_World, loaded_dim, loaded_world), 
                     World_Dimensions (World_Dim, w_width, w_height))

read_world_from_bmp_file :: String 
   -> Attributed_World world 
   -> (Loaded_World -> Attributed_World world) 
   -> IO (Attributed_World world)
read_world_from_bmp_file filename empty_world prepare_world = do
   inputbmp <- readBMP filename
   case inputbmp of
      Left  e   -> hPutStrLn stderr (show e) >> return empty_world -- show error                    
      Right bmp -> return (bmp_to_world bmp prepare_world)         -- return the loaded world

bmp_to_world :: BMP -> (Loaded_World -> Attributed_World world) -> Attributed_World world
bmp_to_world bmp prepare_world = prepare_world 
                     (L_World {  loaded_dim   = World_Dim {w_width = width, w_height = height},
                                 loaded_world = cells_with_coordinates})
   where
      cells_with_coordinates = filter non_empty (zip cells coordinates)
         where
            non_empty (cell, _) = not (cell == Empty) 
            cells = map pixel_to_cell rgba_pixels
               where
                  rgba_pixels = split_into_pixel (unpack (unpackBMPToRGBA32 bmp))
            coordinates = [(x - (width `div` 2), y - (height `div` 2)) 
                              | y <- [0 .. height - 1], x <- [0 .. width - 1]]
      (width, height) = (fromIntegral width', fromIntegral height')
         where
            (width', height') = bmpDimensions bmp           

split_into_pixel :: [Word8] -> [Pixel]
split_into_pixel list = case list of
   r: g: b: a: xs -> Pixel {red = r, green = g, blue = b, alpha = a}: split_into_pixel xs
   []             -> [] 
   _              -> error "bitmap does not spit up into four byte pixels"
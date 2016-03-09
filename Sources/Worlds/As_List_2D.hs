{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

--
-- Uwe R. Zimmer
-- Australia 2012
--

module Worlds.As_List_2D (

   World_model,
   
   World_Class (
      empty_world,        -- :: World_model
      prepare_world,      -- :: List_2D Cell -> World_model
      transition_world,   -- :: World_model -> World_model
      draw_world,         -- :: World_model -> Float -> Picture
      element_occurrence, -- :: Cell -> World_model -> Int
      size                -- ::         World_model -> Int
   )
   
) where

import Data.Cell (Cell)
import Data.List_2D (List_2D, element_occurrence, size)
import Drawing.Constants (cell_size) 
import Drawing.Worlds.For_List_2D (draw_world)
import Transitions.For_List_2D (transition_world)
import World_Class {- ( World_Class (
                        empty_world,
                        prepare_world,
                        transition_world,
                        draw_world,
                        element_occurrence,
                        size),
                     World_Dimensions (World_Dim, w_width, w_height),
                     Loaded_World (loaded_dim, loaded_world),
                     Attributed_World (A_World, world_dim, world_itself),
                     fun_world) -}


type World_model = List_2D Cell

instance World_Class World_model where
   empty_world                     = A_World {world_dim = World_Dim {w_width = 0, w_height = 0}, world_itself = []} 
   prepare_world l_world           = A_World {world_dim = loaded_dim l_world, world_itself = (loaded_world l_world)}
   transition_world a_world        = fun_world Transitions.For_List_2D.transition_world a_world
   draw_world a_world              = Drawing.Worlds.For_List_2D.draw_world (world_itself a_world) (cell_size (world_dim a_world))
   element_occurrence cell a_world = Data.List_2D.element_occurrence cell (world_itself a_world)
   size world                      = Data.List_2D.size (world_itself world)

--
-- Uwe R. Zimmer
-- Australia 2012
--

module Data.Cell (
   Cell (Head, Tail, Conductor, Empty)
) where

data Cell = Head | Tail | Conductor | Empty
   deriving Eq
   
instance Show Cell where
   show cell = case cell of
      Head      -> "H"
      Tail      -> "T"
      Conductor -> "C"
      Empty     -> " "
   
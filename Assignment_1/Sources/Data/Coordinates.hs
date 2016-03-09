--
-- Uwe R. Zimmer
-- Australia 2012
--

module Data.Coordinates (
   Distance,
   X_Coord,
   Y_Coord,
   Coord,
   Element_w_Coord,
) where

type Distance = Integer;
type X_Coord  = Integer;
type Y_Coord  = Integer;

type Coord = (X_Coord, Y_Coord);

type Element_w_Coord e = (e, Coord)


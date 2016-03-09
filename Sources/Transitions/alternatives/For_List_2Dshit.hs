--
-- Uwe R. Zimmer
-- Australia 2012
--

module Transitions.For_List_2D (
   transition_world -- :: List_2D Cell -> List_2D Cell
) where

--import Data.Cell (Cell)
import Data.Cell (Cell (Head, Tail, Conductor, Empty))
import Data.Coordinates (Coord)
import Data.List_2D
import Data.Integer_Subtypes (Nat)
import Transitions.RadixTrie


-- First attempt - O(n) as it goes through the list from start to finish,
-- and O(n) again as it calls functions that are O(n)
-- so this function is O(n squared)
-- yuck!
transition_world :: List_2D Cell -> List_2D Cell
transition_world world = transition_conductors working (create_neighbours heads) conductors
   where (working, heads, conductors) = part_process_world world ([], [], [])
   
   
-- this function accepts the working list of the world, the neighbours radix trie,
-- and the list of conductor coordinates. It returns the complete new world in its entirety.
transition_conductors :: List_2D Cell -> FirstRadixNode (Maybe (FirstRadixNode (Maybe Nat))) -> [Coord] -> List_2D Cell
transition_conductors new_world neighbours conductors = case conductors of
   []                      -> new_world
   c: cs                   -> case count_neighbour c neighbours of
      1                    -> transition_conductors ((Head     , c): new_world) neighbours cs
      2                    -> transition_conductors ((Head     , c): new_world) neighbours cs
      _                    -> transition_conductors ((Conductor, c): new_world) neighbours cs


-- this function will go through the contents of the World list once only, and produce a troople
-- of three lists - the first list is the working New_World, where all Tails -> Conductor and Head -> Tail
-- are included - and Empties dumped. The second list is a list of coordinates of all the Heads (that have been
-- transformed into tails). This list will be used to generate the neighbours Radix Trie. The last list
-- contains coords of all the Conductors, which have not been processed yet. Once the neighbours radix is created,
-- this last list will be processed to produce new Head
--
-- lawl
part_process_world :: List_2D Cell -> (List_2D Cell, [Coord], [Coord]) -> (List_2D Cell, [Coord], [Coord])
part_process_world world (new_world, heads, conductors) = case world of
   []                      -> (new_world, heads, conductors)
   (e, (x, y)): es         -> case e of
      Empty                -> part_process_world es (                      new_world,         heads,         conductors)
      Tail                 -> part_process_world es ((Conductor, (x, y)) : new_world,         heads,         conductors)
      Head                 -> part_process_world es ((     Tail, (x, y)) : new_world, (x, y): heads,         conductors)
      Conductor            -> part_process_world es (                      new_world,         heads, (x, y): conductors)


            
-- construct a RadixTrie of coordinate/elem pairs that neighbour a Head
-- the elem in this case is a Nat that represents the number of Heads
-- that neighbour this cell
create_neighbours :: [Coord] -> FirstRadixNode (Maybe (FirstRadixNode (Maybe Nat)))
create_neighbours heads = create_neighbours_radix heads (FirstRadixNode NullNode NullNode)
      
-- create the Radix Trie using a list of Head-only elements. Hopefully this will kill off the memory issues
create_neighbours_radix :: [Coord] -> FirstRadixNode (Maybe (FirstRadixNode (Maybe Nat))) -> FirstRadixNode (Maybe (FirstRadixNode (Maybe Nat)))
create_neighbours_radix heads working = case heads of
   []                      -> working
   c: cs                   -> create_neighbours_radix cs (increment_coordinates c working)
      
      
-- insert a head into the neighbours data set
count_neighbour :: Coord -> FirstRadixNode (Maybe (FirstRadixNode (Maybe Nat))) -> Integer
count_neighbour (x, y) neighbours = count_recurser neighbour_set neighbours 0
   where neighbour_set = [(a, b) | a <- [x-1..x+1], b <- [y-1..y+1], (a /= x || b /= y)]


-- 
count_recurser :: [Coord] -> FirstRadixNode (Maybe (FirstRadixNode (Maybe Nat))) -> Integer -> Integer
count_recurser coords neighbours working = case coords of
   []          -> working
   c: cs       -> case retrieve_coordinates c neighbours of
      Nothing  -> count_recurser cs neighbours working
      Just 1   -> 1 + count_recurser cs neighbours working
      _        -> error "This is supposed to be unreachable code in count_recurser"



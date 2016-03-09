
-- hard to believe, but RadixTrie4 gives the best results when used on Langton's 11
-- so this is the one I will go with (!)

module Transitions.RadixTrie (
   FirstRadixNode (FirstRadixNode),
   RadixTrieNode (NullNode, RadixTrieNode),
   increment_coordinates,        -- :: (Int, Int) -> RTN (Maybe RTN (Maybe Int)) -> RTN (Maybe RTN (Maybe Int))
   retrieve_coordinates          -- :: (Int, Int) -> RTN (Maybe RTN (Maybe Int)) -> Maybe Int
) where

import Data.Coordinates

data FirstRadixNode a = FirstRadixNode { positiveNode :: RadixTrieNode a,
					 negativeNode :: RadixTrieNode a
					 } deriving (Show)


data RadixTrieNode a = NullNode |
			RadixTrieNode { nodeValue :: a,
					node0 :: RadixTrieNode a,
					node1 :: RadixTrieNode a
					} deriving (Show)


-- the two accessor functions that the main program uses
-- increment coordinates increments the stored count value for the given coordinates
-- retrieve coordinates returns the stored count for the given coordinates
increment_coordinates :: (Num a) => Coord -> FirstRadixNode (Maybe (FirstRadixNode (Maybe a))) -> FirstRadixNode (Maybe (FirstRadixNode (Maybe a)))
increment_coordinates (x, y) supertree = case retrieve_trie supertree x of
	Nothing           -> insert_trie supertree x (increment_trie y (FirstRadixNode NullNode NullNode)) 
	Just (tree)       -> insert_trie supertree x (increment_trie y tree) 


retrieve_coordinates :: (Num a) => Coord -> FirstRadixNode (Maybe (FirstRadixNode (Maybe a))) -> (Maybe a)
retrieve_coordinates (x, y) supertree = case retrieve_trie supertree x of
	Nothing           -> Nothing
	Just (tree)       -> retrieve_trie tree y


-- ********************************************************************
-- PRIVATE Functions
--
-- That's some old school object-oriented talk right there
--
-- These functions form the part of the abstract data type
-- that are hidden away from the user
--
--
-- Insertion functions
--
-- ********************************************************************

-- inserts a value into the radix trie by selecting the correct sub-branch
-- based on the sign of the key, and instructing them to insert
insert_trie :: FirstRadixNode (Maybe a) -> Integer -> a -> FirstRadixNode (Maybe a)
insert_trie supernode key value
	| key < 0            = FirstRadixNode                         (positiveNode  supernode)   (insert_value (abs key) value (negativeNode supernode))
	| key >= 0           = FirstRadixNode (insert_value key value (positiveNode (supernode)))                               (negativeNode supernode)
	| otherwise          = error "Unreachable code in insert_trie"


-- this crazy function will associate a new value with a given key
insert_value :: Integer -> a -> RadixTrieNode (Maybe a) -> RadixTrieNode (Maybe a)	
insert_value key value tree = case (split_key key, tree) of
	-- start by matching up cases where this tree doesnt exist yet
	((0, 0), NullNode)	-> RadixTrieNode Nothing (new_node value NullNode) NullNode
	((1, 0), NullNode)	-> RadixTrieNode Nothing NullNode (new_node value NullNode)


	-- this tree still doesnt exist, but the indexed value is deeper than the next level down
	((0, r), NullNode)	-> RadixTrieNode Nothing (insert_value r value NullNode) NullNode
	((1, r), NullNode)	-> RadixTrieNode Nothing NullNode (insert_value r value NullNode)


	-- ok, now the tree exists and I am searching for my child
	((0, 0), _)		-> RadixTrieNode (nodeValue tree) (new_node value (node0 tree)) (node1 tree)
	((1, 0), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (new_node value (node1 tree))


	-- the tree exists but my target is deeper
	((0, r), _)		-> RadixTrieNode (nodeValue tree) (insert_value r value (node0 tree)) (node1 tree)
	((1, r), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (insert_value r value (node1 tree))


	(     _, _)		-> error "Unreachable code in increment_value."


new_node :: a -> RadixTrieNode (Maybe a) -> RadixTrieNode (Maybe a)
new_node value existing = case existing of
	NullNode		-> RadixTrieNode (Just value) NullNode NullNode
	x			-> RadixTrieNode (Just value) (node0 x) (node1 x)


increment_trie :: (Num a) => Integer -> FirstRadixNode (Maybe a) -> FirstRadixNode (Maybe a)
increment_trie key tree
	| key < 0            = FirstRadixNode                      (positiveNode tree)  (increment_value (abs key) (negativeNode tree))
	| key >= 0           = FirstRadixNode (increment_value key (positiveNode tree))                            (negativeNode tree)
	| otherwise          = error "Unreachable code in increment_trie"


increment_value :: (Num a) => Integer -> RadixTrieNode (Maybe a) -> RadixTrieNode (Maybe a)
increment_value key tree = case (split_key key, tree) of
   -- start by matching up cases where this tree doesnt exist yet
   ((0, 0), NullNode)   -> RadixTrieNode Nothing (new_inc_node NullNode) NullNode
   ((1, 0), NullNode)   -> RadixTrieNode Nothing NullNode (new_inc_node NullNode)


   -- this tree still doesnt exist, but the indexed value is deeper than the next level down
   ((0, r), NullNode)   -> RadixTrieNode Nothing (increment_value r NullNode) NullNode
   ((1, r), NullNode)   -> RadixTrieNode Nothing NullNode (increment_value r NullNode)


   -- ok, now the tree exists and I am searching for my child  
   ((0, 0),        _)   -> RadixTrieNode (nodeValue tree) (new_inc_node $ node0 tree) (node1 tree)
   ((1, 0),        _)   -> RadixTrieNode (nodeValue tree) (node0 tree) (new_inc_node $ node1 tree)


   -- the tree exists but my target is deeper
   ((0, r),        _)   -> RadixTrieNode (nodeValue tree) (increment_value r (node0 tree)) (node1 tree)
   ((1, r),        _)   -> RadixTrieNode (nodeValue tree) (node0 tree) (increment_value r (node1 tree))


   (     _,        _)   -> error "Unreachable code in increment_value."



new_inc_node :: (Num a) => RadixTrieNode (Maybe a) -> RadixTrieNode (Maybe a)
new_inc_node existing = case existing of
   NullNode       -> RadixTrieNode (Just 1) NullNode NullNode
   x              -> case nodeValue x of
      Nothing		-> RadixTrieNode (Just 1)       (node0 x) (node1 x)
      Just y		-> RadixTrieNode (Just (1 + y)) (node0 x) (node1 x)



-- ********************************************************************
-- the retrieval functions
--
-- ********************************************************************
--
-- returns the branch of the trie that corresponds to the sign of the key
-- positive and negative keys are stored in seperate branches
-- for what i hope are obvious reasons
retrieve_trie :: FirstRadixNode (Maybe a) -> Integer -> Maybe a
retrieve_trie tree key
	| key < 0            = retrieve_value (abs key) (negativeNode tree)
	| key >= 0           = retrieve_value      key  (positiveNode tree)
	| otherwise          = error "Unreachable code in retrieve_trie"


-- returns the value, if any, associated with the given index
-- in the context of this assignment, that may well be another
-- FirstRadixNode data structure
retrieve_value :: Integer -> RadixTrieNode (Maybe a) -> Maybe a
retrieve_value key tree = case (split_key key, tree) of
	(     _, NullNode)	-> Nothing
	((n, 0),        _)   -> get_node_value   (get_tree_node n tree)
	((n, r),        _)   -> retrieve_value r (get_tree_node n tree)


-- yes, this function would have made an excellent candidate for a where clause baby
get_tree_node :: Integer -> RadixTrieNode (Maybe a) -> RadixTrieNode (Maybe a)
get_tree_node index tree = case index of
   0                    -> node0 tree
   1                    -> node1 tree
   _                    -> NullNode


-- get whatever's stored in the given node
get_node_value :: RadixTrieNode (Maybe a) -> Maybe a
get_node_value node = case node of
	NullNode             -> Nothing
	x                    -> nodeValue x


-- ********************************************************************
-- The mathe-magic function
-- That makes efficient storage and retrieval possible
--
-- ********************************************************************

split_key :: Integer -> (Integer, Integer)
split_key key = (key `mod` 2, key `div` 2)

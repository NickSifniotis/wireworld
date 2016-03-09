
module Transitions.RadixTrie (
	FirstRadixNode (FirstRadixNode),
	RadixTrieNode (NullNode, RadixTrieNode),
	increment_coordinates,
	retrieve_coordinates
) where

data FirstRadixNode a = FirstRadixNode { positiveNode :: RadixTrieNode a,
					 negativeNode :: RadixTrieNode a
					 } deriving (Show)


data RadixTrieNode a = NullNode |
			RadixTrieNode { nodeValue :: a,
					node0 :: RadixTrieNode a,
					node1 :: RadixTrieNode a,
					node2 :: RadixTrieNode a,
					node3 :: RadixTrieNode a,
					node4 :: RadixTrieNode a,
					node5 :: RadixTrieNode a,
					node6 :: RadixTrieNode a,
					node7 :: RadixTrieNode a
					} deriving (Show)



split_key :: Integer -> (Integer, Integer)
split_key key = (key `mod` 8, key `div` 8)


increment_coordinates :: (Num a) => (Integer, Integer) -> FirstRadixNode (Maybe (FirstRadixNode (Maybe a))) -> FirstRadixNode (Maybe (FirstRadixNode (Maybe a)))
increment_coordinates (x, y) supertree = case retrieve_trie supertree x of
	Nothing 		-> insert_trie supertree x (increment_trie y (FirstRadixNode NullNode NullNode)) 
	Just (tree)		-> insert_trie supertree x (increment_trie y tree) 


retrieve_coordinates :: (Num a) => (Integer, Integer) -> FirstRadixNode (Maybe (FirstRadixNode (Maybe a))) -> (Maybe a)
retrieve_coordinates (x, y) supertree = case retrieve_trie supertree x of
	Nothing			-> Nothing
	Just (tree)		-> retrieve_trie tree y


retrieve_trie :: FirstRadixNode (Maybe a) -> Integer -> Maybe a
retrieve_trie tree key
	| key < 0	= retrieve_value (abs key) (negativeNode tree)
	| key >= 0	= retrieve_value key (positiveNode tree)
	| otherwise 	= error "Unreachable code in retrieve_trie"

insert_trie :: FirstRadixNode (Maybe a) -> Integer -> a -> FirstRadixNode (Maybe a)
insert_trie supernode key value
	| key < 0	= FirstRadixNode (positiveNode supernode) (insert_value (abs key) value (negativeNode supernode))
	| key >= 0	= FirstRadixNode (insert_value key value (positiveNode (supernode))) (negativeNode supernode)
	| otherwise	= error "Unreachable code in insert_trie"


insert_value :: Integer -> a -> RadixTrieNode (Maybe a) -> RadixTrieNode (Maybe a)	
insert_value key value tree = case (split_key key, tree) of
	-- start by matching up cases where this tree doesnt exist yet
	((0, 0), NullNode)	-> RadixTrieNode Nothing (new_node value NullNode) NullNode NullNode NullNode NullNode NullNode NullNode NullNode
	((1, 0), NullNode)	-> RadixTrieNode Nothing NullNode (new_node value NullNode) NullNode NullNode NullNode NullNode NullNode NullNode
	((2, 0), NullNode)	-> RadixTrieNode Nothing NullNode NullNode (new_node value NullNode) NullNode NullNode NullNode NullNode NullNode
	((3, 0), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode (new_node value NullNode) NullNode NullNode NullNode NullNode
	((4, 0), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode NullNode (new_node value NullNode) NullNode NullNode NullNode
	((5, 0), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode NullNode NullNode (new_node value NullNode) NullNode NullNode
	((6, 0), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode NullNode NullNode NullNode (new_node value NullNode) NullNode
	((7, 0), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode NullNode NullNode NullNode NullNode (new_node value NullNode)

	-- this tree still doesnt exist, but the indexed value is deeper than the next level down
	((0, r), NullNode)	-> RadixTrieNode Nothing (insert_value r value NullNode) NullNode NullNode NullNode NullNode NullNode NullNode NullNode
	((1, r), NullNode)	-> RadixTrieNode Nothing NullNode (insert_value r value NullNode) NullNode NullNode NullNode NullNode NullNode NullNode
	((2, r), NullNode)	-> RadixTrieNode Nothing NullNode NullNode (insert_value r value NullNode) NullNode NullNode NullNode NullNode NullNode
	((3, r), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode (insert_value r value NullNode) NullNode NullNode NullNode NullNode
	((4, r), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode NullNode (insert_value r value NullNode) NullNode NullNode NullNode
	((5, r), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode NullNode NullNode (insert_value r value NullNode) NullNode NullNode
	((6, r), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode NullNode NullNode NullNode (insert_value r value NullNode) NullNode
	((7, r), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode NullNode NullNode NullNode NullNode (insert_value r value NullNode)

	-- ok, now the tree exists and I am searching for my child
	((0, 0), _)		-> RadixTrieNode (nodeValue tree) (new_node value (node0 tree)) (node1 tree) (node2 tree) (node3 tree) (node4 tree) (node5 tree) (node6 tree) (node7 tree)
	((1, 0), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (new_node value (node1 tree)) (node2 tree) (node3 tree) (node4 tree) (node5 tree) (node6 tree) (node7 tree)
	((2, 0), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (new_node value (node2 tree)) (node3 tree) (node4 tree) (node5 tree) (node6 tree) (node7 tree)
	((3, 0), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (new_node value (node3 tree)) (node4 tree) (node5 tree) (node6 tree) (node7 tree)
	((4, 0), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (node3 tree) (node4 tree) (new_node value (node5 tree)) (node6 tree) (node7 tree)
	((5, 0), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (node3 tree) (node4 tree) (new_node value (node5 tree)) (node6 tree) (node7 tree)
	((6, 0), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (node3 tree) (node4 tree) (node5 tree) (new_node value (node6 tree)) (node7 tree)
	((7, 0), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (node3 tree) (node4 tree) (node5 tree) (node6 tree) (new_node value (node7 tree))

	-- the tree exists but my target is deeper
	((0, r), _)		-> RadixTrieNode (nodeValue tree) (insert_value r value (node0 tree)) (node1 tree) (node2 tree) (node3 tree) (node4 tree) (node5 tree) (node6 tree) (node7 tree)
	((1, r), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (insert_value r value (node1 tree)) (node2 tree) (node3 tree) (node4 tree) (node5 tree) (node6 tree) (node7 tree)
	((2, r), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (insert_value r value (node2 tree)) (node3 tree) (node4 tree) (node5 tree) (node6 tree) (node7 tree)
	((3, r), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (insert_value r value (node3 tree)) (node4 tree) (node5 tree) (node6 tree) (node7 tree)
	((4, r), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (node3 tree) (insert_value r value (node4 tree)) (node5 tree) (node6 tree) (node7 tree)
	((5, r), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (node3 tree) (node4 tree) (insert_value r value (node5 tree)) (node6 tree) (node7 tree)
	((6, r), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (node3 tree) (node4 tree) (node5 tree) (insert_value r value (node6 tree)) (node7 tree)
	((7, r), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (node3 tree) (node4 tree) (node5 tree) (node6 tree) (insert_value r value (node7 tree))

	(     _, _)		-> error "Unreachable code in increment_value."


new_node :: a -> RadixTrieNode (Maybe a) -> RadixTrieNode (Maybe a)
new_node value existing = case existing of
	NullNode		-> RadixTrieNode (Just value) NullNode NullNode NullNode NullNode NullNode NullNode NullNode NullNode
	x			-> RadixTrieNode (Just value) (node0 x) (node1 x) (node2 x) (node3 x) (node4 x) (node5 x) (node6 x) (node7 x)



-- retrieve_value is a polymorphic function, and with good reason
-- the best way to store two-dimensional data, like coordinates,
-- is with a tree of trees. The alternative was to find some way of
-- encoding the two dimensions in the one number, but that would have resulted
-- in a large number of large number calcualtions, as well as imposing a limit
-- on the size of the x- and y- coordinates.
--
-- So the final data structure will be a tree of trees of integers
retrieve_value :: Integer -> RadixTrieNode (Maybe a) -> Maybe a
retrieve_value key tree = case (split_key key, tree) of
	(     _, NullNode)	-> Nothing

	((0, 0), _)		-> get_node_value (node0 tree)
	((1, 0), _)		-> get_node_value (node1 tree)
	((2, 0), _)		-> get_node_value (node2 tree)
	((3, 0), _)		-> get_node_value (node3 tree)
	((4, 0), _)		-> get_node_value (node4 tree)
	((5, 0), _)		-> get_node_value (node5 tree)
	((6, 0), _)		-> get_node_value (node6 tree)
	((7, 0), _)		-> get_node_value (node7 tree)

	((0, r), _)		-> retrieve_value r (node0 tree)
	((1, r), _)		-> retrieve_value r (node1 tree)
	((2, r), _)		-> retrieve_value r (node2 tree)
	((3, r), _)		-> retrieve_value r (node3 tree)
	((4, r), _)		-> retrieve_value r (node4 tree)
	((5, r), _)		-> retrieve_value r (node5 tree)
	((6, r), _)		-> retrieve_value r (node6 tree)
	((7, r), _)		-> retrieve_value r (node7 tree)

	(     _, _)		-> error "Unreachable code in retrieve_value"


get_node_value :: RadixTrieNode (Maybe a) -> Maybe a
get_node_value node = case node of
	NullNode		-> Nothing
	x			-> nodeValue x


increment_trie :: (Num a) => Integer -> FirstRadixNode (Maybe a) -> FirstRadixNode (Maybe a)
increment_trie key tree
	| key < 0		= FirstRadixNode (positiveNode tree) (increment_value (abs key) (negativeNode tree))
	| key >= 0		= FirstRadixNode (increment_value key (positiveNode tree)) (negativeNode tree)
	| otherwise		= error "Unreachable code in increment_trie"


increment_value :: (Num a) => Integer -> RadixTrieNode (Maybe a) -> RadixTrieNode (Maybe a)
increment_value key tree = case (split_key key, tree) of
	-- start by matching up cases where this tree doesnt exist yet
	((0, 0), NullNode)	-> RadixTrieNode Nothing (new_inc_node NullNode) NullNode NullNode NullNode NullNode NullNode NullNode NullNode
	((1, 0), NullNode)	-> RadixTrieNode Nothing NullNode (new_inc_node NullNode) NullNode NullNode NullNode NullNode NullNode NullNode
	((2, 0), NullNode)	-> RadixTrieNode Nothing NullNode NullNode (new_inc_node NullNode) NullNode NullNode NullNode NullNode NullNode
	((3, 0), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode (new_inc_node NullNode) NullNode NullNode NullNode NullNode
	((4, 0), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode NullNode (new_inc_node NullNode) NullNode NullNode NullNode
	((5, 0), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode NullNode NullNode (new_inc_node NullNode) NullNode NullNode
	((6, 0), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode NullNode NullNode NullNode (new_inc_node NullNode) NullNode
	((7, 0), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode NullNode NullNode NullNode NullNode (new_inc_node NullNode)

	-- this tree still doesnt exist, but the indexed value is deeper than the next level down
	((0, r), NullNode)	-> RadixTrieNode Nothing (increment_value r NullNode) NullNode NullNode NullNode NullNode NullNode NullNode NullNode
	((1, r), NullNode)	-> RadixTrieNode Nothing NullNode (increment_value r NullNode) NullNode NullNode NullNode NullNode NullNode NullNode
	((2, r), NullNode)	-> RadixTrieNode Nothing NullNode NullNode (increment_value r NullNode) NullNode NullNode NullNode NullNode NullNode
	((3, r), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode (increment_value r NullNode) NullNode NullNode NullNode NullNode
	((4, r), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode NullNode (increment_value r NullNode) NullNode NullNode NullNode
	((5, r), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode NullNode NullNode (increment_value r NullNode) NullNode NullNode
	((6, r), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode NullNode NullNode NullNode (increment_value r NullNode) NullNode
	((7, r), NullNode)	-> RadixTrieNode Nothing NullNode NullNode NullNode NullNode NullNode NullNode NullNode (increment_value r NullNode)

	-- ok, now the tree exists and I am searching for my child
	((0, 0), _)		-> RadixTrieNode (nodeValue tree) (new_inc_node $ node0 tree) (node1 tree) (node2 tree) (node3 tree) (node4 tree) (node5 tree) (node6 tree) (node7 tree)
	((1, 0), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (new_inc_node $ node1 tree) (node2 tree) (node3 tree) (node4 tree) (node5 tree) (node6 tree) (node7 tree)
	((2, 0), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (new_inc_node $ node2 tree) (node3 tree) (node4 tree) (node5 tree) (node6 tree) (node7 tree)
	((3, 0), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (new_inc_node $ node3 tree) (node4 tree) (node5 tree) (node6 tree) (node7 tree)
	((4, 0), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (node3 tree) (new_inc_node $ node4 tree) (node5 tree) (node6 tree) (node7 tree)
	((5, 0), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (node3 tree) (node4 tree) (new_inc_node $ node5 tree) (node6 tree) (node7 tree)
	((6, 0), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (node3 tree) (node4 tree) (node5 tree) (new_inc_node $ node6 tree) (node7 tree)
	((7, 0), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (node3 tree) (node4 tree) (node5 tree) (node6 tree) (new_inc_node $ node7 tree)

	-- the tree exists but my target is deeper
	((0, r), _)		-> RadixTrieNode (nodeValue tree) (increment_value r (node0 tree)) (node1 tree) (node2 tree) (node3 tree) (node4 tree) (node5 tree) (node6 tree) (node7 tree)
	((1, r), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (increment_value r (node1 tree)) (node2 tree) (node3 tree) (node4 tree) (node5 tree) (node6 tree) (node7 tree)
	((2, r), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (increment_value r (node2 tree)) (node3 tree) (node4 tree) (node5 tree) (node6 tree) (node7 tree)
	((3, r), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (increment_value r (node3 tree)) (node4 tree) (node5 tree) (node6 tree) (node7 tree)
	((4, r), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (node3 tree) (increment_value r (node4 tree)) (node5 tree) (node6 tree) (node7 tree)
	((5, r), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (node3 tree) (node4 tree) (increment_value r (node5 tree)) (node6 tree) (node7 tree)
	((6, r), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (node3 tree) (node4 tree) (node5 tree) (increment_value r (node6 tree)) (node7 tree)
	((7, r), _)		-> RadixTrieNode (nodeValue tree) (node0 tree) (node1 tree) (node2 tree) (node3 tree) (node4 tree) (node5 tree) (node6 tree) (increment_value r (node7 tree))

	(     _, _)		-> error "Unreachable code in increment_value."



new_inc_node :: (Num a) => RadixTrieNode (Maybe a) -> RadixTrieNode (Maybe a)
new_inc_node existing = case existing of
	NullNode		-> RadixTrieNode (Just 1) NullNode NullNode NullNode NullNode NullNode NullNode NullNode NullNode
	x			-> case nodeValue x of
		Nothing		-> RadixTrieNode (Just 1) (node0 x) (node1 x) (node2 x) (node3 x) (node4 x) (node5 x) (node6 x) (node7 x)
		Just y		-> RadixTrieNode (Just (1 + y)) (node0 x) (node1 x) (node2 x) (node3 x) (node4 x) (node5 x) (node6 x) (node7 x)

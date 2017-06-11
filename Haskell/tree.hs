data Tree a = Empty | Node a (Tree a) (Tree a)
		deriving(Eq,Ord)

instance Show a => Show (Tree a) where
	show Empty = " "
	show (Node value Empty Empty) = show value 
	show (Node value left Empty) = show value ++ " " ++ show left 
	show (Node value left right) = show value ++ " " ++ show left ++ " "++ show right

example = (Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) Empty))

instance Functor Tree where
	fmap fun Empty = Empty
	fmap fun tree@(Node value left Empty) = Node (fun value) (fmap fun left) Empty
	fmap fun tree@(Node value left right) = Node (fun value) (fmap fun left) (fmap fun right)

createTree 0 value = Empty
createTree 1 value = (Node value Empty Empty)
createTree init value = (Node value (createTree (init-1) value) (createTree (init-1) value))

insert a Empty = (Node a Empty Empty)
insert a tree@(Node value (Empty) (Empty)) = (Node value (Node a Empty Empty) (Empty)) 
insert a tree@(Node value left right) 
	| a <= value = (Node value (insert a left) right)
	| a > value = (Node value left (insert a right))

search a Empty = False
search a tree@(Node value left right) 
	| value == a = True
	| a < value = search a left
	| a > value = search a right

nnodes Empty = 0
nnodes tree@(Node value left right) = nnodes left + nnodes right + 1

nsum Empty = 0
nsum tree@(Node value left right) = value + nsum left + nsum right

leaves Empty = []
leaves tree@(Node value Empty Empty) = [value]
leaves tree@(Node value left right) = leaves left ++ leaves right

tmap op Empty = Empty
tmap op tree@(Node value left right) = Node (op value) (tmap op left) (tmap op right)

tremove a Empty = Empty
tremove a tree@(Node value Empty Empty)
	| value == a = Empty
tremove a tree@(Node value Empty right)
	| value == a = right
tremove a tree@(Node value left Empty)
	| value == a = left
tremove a tree@(Node value left right)
	| value == a = (Node (nextValue right) left (tremove (nextValue right) right))
	| a < value = (Node value (tremove a left) right)
	| a > value = (Node value left (tremove a right))

nextValue tree@(Node value Empty right) = value
nextValue tree@(Node value left right) = nextValue left

isBalanced tree@(Node value Empty Empty) = True
isBalanced tree@(Node value left Empty) = False
isBalanced tree@(Node value Empty right) = False
isBalanced tree@(Node value left right) = isBalanced left && isBalanced right

isEmpty tree@(Node Empty Empty Empty) = True

--isBinary Empty = True
--isBinary tree@(Node value Empty Empty) = True
--isBinary tree@(Node value left Empty) = isBinary left
--isBinary tree@(Node value Empty right) = isBinary right
--isBinary tree@(Node value left right) = isBinary left && isBinary right



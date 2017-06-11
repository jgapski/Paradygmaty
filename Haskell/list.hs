data List a = Empty | Cons a (List a) 
		deriving (Eq,Ord)

instance Show a => Show (List a) where
	show Empty = "Empty"
	show (Cons value Empty) = show value ++ " "
	show (Cons value lista) = show value ++ " " ++ show lista

example = (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Empty)))))
functo = Cons (+1) (Cons (+2) Empty)

instance Functor List where
	fmap fun Empty = Empty
	fmap fun list@(Cons value Empty) = Cons (fun value) Empty
	fmap fun list@(Cons value lista) = Cons (fun value) (fmap fun lista)

instance Applicative List where
	pure a = (Cons a Empty)
	Empty <*> list = Empty
	fun@(Cons value Empty) <*> list = fmap value list
	fun@(Cons value lista) <*> list = (fmap value list) +++ (lista <*> list)

lmap op Empty = Empty
lmap op list@(Cons value lista) = Cons (op value) (lmap op lista)

infixl 5 +++
Empty +++ Empty = Empty
Empty +++ list@(Cons value lista) = list
list@(Cons value lista) +++ Empty = list
list1@(Cons value1 lista1) +++ list2@(Cons value2 lista2) = Cons value1 (lista1+++list2)

lzipwith op Empty Empty = Empty
lzipwith op list1@(Cons value1 lista1) list2@(Cons value2 lista2) = Cons (op value1 value2) (lzipwith op lista1 lista2)

fromNormalList [] = Empty
fromNormalList (x:xs) = (Cons x (fromNormalList xs))

toNormalList Empty = []
toNormalList list@(Cons value lista) = value:(toNormalList lista)

lfoldl op init Empty = init 
lfoldl op init list@(Cons value lista) = lfoldl op (op init value) lista

lreverse Empty = Empty
lreverse list@(Cons value lista) = (lreverse lista) +++ (Cons value Empty)

lfoldr op init Empty = init
lfoldr op init list@(Cons value lista) = lfoldr op init (lreverse list)




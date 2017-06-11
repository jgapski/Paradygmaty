kropka f g = \x -> f (g x)

c x = x+1
d x = x+5

infixl 9 -..
f -.. g = \x -> f (g x)

data Tree a = E|L a|N (Tree a) a (Tree a)
	deriving(Eq,Ord,Read)

instance Show a => Show (Tree a) where
	show E = " "
	show (L value) = "Lisc: " ++ show value
	show (N E value E) = show value
	show (N left value E) = show left ++ show value 
	show (N E value right) = show value ++ show right
	show (N left value right) = show left ++ show value ++ show right

instance Functor Tree where
	fmap fun E = E
	fmap fun (L a) = L (fun a)
	fmap fun (N left a E) = N (fmap fun left) (fun a) E
	fmap fun (N left a right) = N (fmap fun left) (fun a) (fmap fun right)	

przyklad = (N (E) E (E))


licz E = 0
licz tree@(L a) = 0
licz tree@(N left value right) 
	| value == E = 0
	| otherwise = (licz left) + (licz right) + 1

pobierz = getLine >>= \ a ->
	  getLine >>= \ b ->
	  getLine >>= \ c ->
	  putStrLn(reverse a ++ "\n" ++ reverse b ++ "\n" ++ reverse c)

main = reverse <$> getLine >>= putStrLn

przyklad2 = do {putStrLn "Czesc"; x <- getLine ; putStr x; putChar '\n' }

main2 = putStrLn "Czesc" >> ( getLine >>= putStrLn)

data List a = Empty|Cons a (List a)
	deriving(Show,Read,Eq,Ord)

instance Functor List where
	fmap fun Empty = Empty
	fmap fun list@(Cons value Empty) = Cons (fun value) Empty
	fmap fun list@(Cons value next) = Cons (fun value) (fmap fun next)

instance Applicative List where
	pure a = (Cons a Empty)
	Empty <*> list = Empty
	fun@(Cons value lista) <*> list = (fmap value list) .+ (lista <*> list)

infixl 9 .+
Empty .+ Empty = Empty
Empty .+ list1@(Cons value lista1) = list1
list1@(Cons value lista1) .+ Empty = list1
list1@(Cons value1 lista1) .+ list2@(Cons value2 lista2) = Cons (value1) (lista1 .+ list2)

data MMaybe a = NNothing | JJust a 
	deriving(Show,Eq,Read,Ord)

instance Functor MMaybe where
	fmap fun NNothing = NNothing
	fmap fun (JJust a) = JJust (fun a)

instance Applicative MMaybe where
	pure a = JJust a
	(JJust a) <*> m = (fmap a m)
	NNothing <*> m = NNothing

instance Monad MMaybe where
	return a = JJust a
	fail _ = NNothing
	JJust a >>= fun = fun a
	NNothing >>= fun = NNothing

wielomian list arg = foldl (+) 0 (map (\ x -> arg^((length list) - (fst x)-1) * (snd x)) (zip [0..] list))
	

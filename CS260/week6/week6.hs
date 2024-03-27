{- Functions and Folds! ---} 

data List a = Cons a (List a) | Empty
  deriving Show

myMap :: (a -> b) -> List a -> List b
myMap f Empty = Empty
myMap f (Cons x xs) = Cons (f x) (myMap f xs)

instance Functor List where
  fmap = myMap

data Nullable a = Null | NotNull a
  deriving Show

instance Functor Nullable where
  fmap f Null = Null
  fmap f (NotNull x) = NotNull (f x)

data BTree a = Leaf a | Node (BTree a) (BTree a)
  deriving Show

{- instance Functor BTree where
  fmap f Leaf = Leaf
  fmap f (Node lt x rt) = Node (fmap f lt) (f x) (fmap f rt) 

  -} 

lengthList :: [a] -> Int
lengthList [] = 0
lengthList (x:xs) = 1 + lengthList xs

productList :: Num a => [a] -> a
productList [] = 1
productList (x:xs) = x * productList xs

sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs

length1 xs = foldr (\x acc -> acc + 1) 0 xs

-- foldr f acc [1,2,3,4] $[]

-- acc :: [a] -> [a]
-- acc $ [] == []
-- acc x = x
-- acc == id

{- 
foldl f acc [1,2,3,4} =
(((acc 'f' 1) 'f' 2 ) 'f' 3) 'f' 4

fxy = x'f'y

foldr f acc [1,2,3,4] =
1 'f' (2 'f' (3 'f' (4 'f' acc)))

foldr f acc [4,3,2,1] =
4 'f' (3 'f' (2 'f' (1 'f' acc)))

 -}
{-type 'cd', then ' stack ghci nameOfFile ' to load module -}

lenthInt :: [int] -> Int
lenthInt [] = 0
lenthInt (x:xs) = 1 + lenthInt xs

lengthString :: [String] -> Int
lengthString [] = 0
lengthString (x:xs) = 1 + lengthString xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

myFun :: a -> a
myFun x = x

myFun2 :: a -> b
myFun2 a = undefined -- undefined is a function that returns a bottom value, it is used to indicate that a function is not yet implemented

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

try :: Num a => a -> a -> a
try x y = x + y 

tryEq :: Eq a => a -> a -> Bool
tryEq x y = x == y

{- Dictionary Lookup ..  -}

myLookup :: [(a,b)] -> a -> b
myLookup [] _ = error "Empty List"
myLookup ((x,y):xs) a | x == a = y
                      | otherwise = myLookup xs a

{- Dictionary Insertion -}

myInsert :: Eq a => [(a,b)] -> a -> b -> [(a,b)]
myInsert [] a b = [(a,b)]
myInsert ((x,y):xs) a b | x == a = (a,b):xs
                        | otherwise = (x,y) : myInsert xs a b

{- Dictionary Deletion -}

myDelete :: Eq a => [(a,b)] -> a -> [(a,b)]
myDelete [] _ = []
myDelete ((x,y):xs) a | x == a = xs
                      | otherwise = (x,y) : myDelete xs a

{- Dictionary Update -}

myUpdate :: Eq a => [(a,b)] -> a -> b -> [(a,b)]
myUpdate [] a b = [(a,b)]
myUpdate ((x,y):xs) a b | x == a = (a,b) : xs
                        | otherwise = (x,y) : myUpdate xs a b

{- Dictionary Lookup with Default -}

myLookupDefault :: Eq a => [(a,b)] -> b -> a -> b
myLookupDefault [] b _ = b
myLookupDefault ((x,y):xs) b a | x == a = y
                               | otherwise = myLookupDefault xs b a

{- Dictionary Insertion with Default -}


{-Higher order functions-
Labs 1-3 pm in LT1201 on Friday
Quiz Haskell in Use
-Ord vs Enum
-}

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f _ = []
myFilter f (x:xs) | f x = x : myFilter f xs
                   | otherwise = myFilter f xs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f _ [] = []
myZipWith f [] _ = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
{-ghci ... zipWith (\x y -> (x. y) [1..5] [6..10] ) 
ghci> zipWith ($) [\x -> x + 1. \x  ->x * 2] [6,7] -}

myFun :: Int -> Int -> Int
myFun x y = x + y
{-ghci> myFun 1 2 -} 
{- myFun x = \y -> x + y -}

{- ghci... map (myFun 3) [1..5] -}

{-myOtherFun :: (Int, Int) -> Int
myOtherFun (x, y) = x + y -}

twice :: (a -> a) -> a -> a
twice f x = f (f x)
{-ghci> twice (\x -> x + 1) 1-}
{-ghci> twice (\x -> x ++ "hello") "world"-}

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 f x = x
nTimes n f x = f (nTimes (n-1) f x)

myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f (x, y)

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (x, y) = f x y

fix :: (a -> a) -> a
fix f = f (fix f)

{-
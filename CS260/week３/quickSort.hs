{-
Quicksort algorithm roughly
1. if listis empty then done (or one element long)
2. select pivot element
3. sort list around pivot element, less than on the left, greater than on the right
of the pivot element
4. until every element has been sorted
-}

lessThan :: Ord a => a -> [a] -> [a]
lessThan _ [] = []
lessThan a (x:xs) | x < a = x : lessThan a xs
                  | otherwise = lessThan a xs

moreThan :: Ord a => a -> [a] -> [a]
moreThan _ [] = []
moreThan a (x:xs) | x > a = x : moreThan a xs
                  | otherwise = moreThan a xs
{-where x will be our pivot element -}
quickIshSort :: Ord a => [a] -> [a]
quickIshSort [] = []
quickIshSort (x:xs) = quickIshSort (lessThan x xs) ++ [x] ++ quickIshSort (moreThan x xs)
    
    {- This is a more efficient version of quickIshsort, it uses a list comprehension to sort the list -}

quickIshSort' :: Ord a => [a] -> [a]
quickIshSort' [] = []
quickIshSort' (x:xs) = quickIshSort' [y | y <- xs, y < x] ++ [x] ++ quickIshSort' [y | y <- xs, y > x]

{- This is a more efficient version of quickIshsort, it uses a list comprehension to sort the list -}

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

{- This is a more efficient version of quickIshsort, it uses a list comprehension to sort the list -}

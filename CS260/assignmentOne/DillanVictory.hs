{-Assingment 1
-------------------------------------------
Submit you answers using the myplace link as  <Your name>.hs file. For all questions you may not change the type of the function. You may define helper functions for any question. You may use any functions within the Haskell prelude unless otherwise specified but you should not import any other libraries. Solutions must be submitted via the myplace link by 01/03/24 17:00. Partial solutions may still attract marks. If your code fails to compile your mark will be capped at 50%. You can check that your code compiles by loading it into ghci using the :l <yourfilename> command.   -}
{- I watched all 43 of this guys videos: https://www.youtube.com/watch?v=Vgu82wiiZ90&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&index=1&ab_channel=PhilippHagenlocher
They were incredibly helpful maybe you should consider sharing them with other students

Dillan R. Victory Coursework CS260 submission 27 Febraury 2024
-} 
  
{- 
1) Define buildSquare which builds the list of numbers whose square is less than a given value. e.g. buildSquare 50 = [1,2,3,4,5,6,7] , buildSquare 10 = [1,2,3] (2 marks)
buildSquare :: Int -> [Int]
buildSquare = undefined 

Idea: 
buildSquare :: Int -> [Int]
buildSquare n = [x | x <- [1..n], x*x < n]

But this not very efficient since it will keep going until n, so we can instead
calculate our upper limit with sqrt and floor to ensure its an Integer
Use fromIntegral because srt expects a floating point number

floor gets largest integer less than or equal to the result

-}

buildSquare :: Int -> [Int]
buildSquare n = [x | x <- [1..upperLimit], x*x < n]
  where upperLimit = floor (sqrt (fromIntegral n))


{-
2) Define longerList which takes 2 lists and returns True if the first is longer than the second and False otherwise. (2 marks)
longerList :: [a] -> [a] -> Bool
longerList = undefined

recursively compare the two lists 

-}

longerList :: [a] -> [a] -> Bool
longerList [] [] = False -- Both list empty so the first cant be longer
longerList _ [] = True -- The second list empty, and the first is not, so the first is longer
longerList [] _ = False -- The first list empty, so it cannot be longer than the second
longerList (_:xs) (_:ys) = longerList xs ys -- Remove one element from each list and compare rest


{-
3a) Define allTrue which returns True only when every element in a list of Booleans is True. (2 marks)

allTrue :: [Bool] -> Bool
allTrue = undefined

If all elements are true the && operator should keep the true through the whole list

-}

allTrue :: [Bool] -> Bool
allTrue = foldr (&&) True


{-
3b) Define smallerNumbers which takes an integer and a list of integers. smallerNumbers should then remove all elements of the list which are larger than the given integer. (2 marks)

smallerNumbers :: Int -> [Int] -> [Int]
smallerNumbers = undefined 

Using list comphrehension to filter out the elements that are larger than the given integer
returns a copy of the list from x:xs where x is less than or equal to n

-}

smallerNumbers :: Int -> [Int] -> [Int]
smallerNumbers n xs = [x | x <- xs, x <= n]



{-
4a) Write a function insert that takes as its first argument a function f, as second argument an element x of a, and as third argument a list xs of type a, and inserts x just before the first element y of xs such that f y > f x (if such a y does not exist, x should be inserted at the end of the list). (2 marks)

insert :: Ord b => (a -> b) -> a -> [a] -> [a]
insert = undefined

use recursion to apply this logic across the list

-}

insert :: Ord b => (a -> b) -> a -> [a] -> [a]
insert f x [] = [x]  -- Base case: If the list is empty, insert x into it
insert f x (y:ys)
    | f y > f x = x : y : ys  -- If f y is greater than f x, insert x before y
    | otherwise = y : insert f x ys  -- Otherwise, keep looking in the rest of the list


{-
4b) Use insert to define a function inssort that sorts a given list such that the sorted list satisfies the following condition: x occurs before y implies f x <= f y (3 marks)

inssort :: Ord b => (a -> b) -> [a] -> [a]
inssort = undefined

takes f and list to sort then uses foldr reducing list from right
uses insertByF to insert at correct position
insertByF f x ys inserts x into sorted list ys based on the comparison function f being applied to both x and ys
recursively comparing f x with f y in the list for each element y in ys 
placing x before y if f x is less than or equal to f y

-} 

inssort :: Ord b => (a -> b) -> [a] -> [a]
inssort f = foldr (insertByF f) []

-- Helper function to insert an element into the sorted list based on the comparison function
insertByF :: Ord b => (a -> b) -> a -> [a] -> [a]
insertByF _ x [] = [x]
insertByF f x (y:ys)
    | f x <= f y = x : y : ys
    | otherwise  = y : insertByF f x ys


{-
5) Define runningSum which returns the sum of a list at every postion. For example runningSum [1,2,3,4,5] == [1, 3, 6, 10, 15]. (3 marks)

runningSum :: [Int] -> [Int]
runningSum = undefined

----------------------------------
data Bit = O | I
 deriving (Show,Eq) 

 Initial idea:

 runningSum :: [Int] -> [Int]
runningSum = scanl (+) 0 . tail . scanl (+) 0

However double scan causes this to be unncecessarily complex

scanl1 (+) takes the binary function and applies it to the first two elements of the list
then it applies it to the result and the next element yada yada yada 
-}

runningSum :: [Int] -> [Int]
runningSum = scanl1 (+)

----------------------------------
data Bit = O | I
 deriving (Show,Eq) 

{-
Here the Bit type has been declared as either an I value or a O value. We will use this type in some of the following questions, you can view it as being extremely similar to the Bool type. Treat I as the bit value 1 and O as 0. Here is an example of a not function. 
-}

myNot :: Bit -> Bit
myNot I = O 
myNot O = I 

{-
6a) Define myAnd which applies logical and to two given Bit values. e.g myAnd I I = I, myAnd O I = O. (1 mark)
myAnd :: Bit ->  Bit -> Bit
myAnd = undefined

the I I case is the only one that returns I, so we can use pattern matching to check for that
_ _ will match any other case

-}

myAnd :: Bit -> Bit -> Bit
myAnd I I = I
myAnd _ _ = O


{-
6b) We can use lists of bits to represent binary numbers. Using myAnd define bitwiseAnd which takes two lists of Bits and applies myAnd to the corresponding elements of each list. Your bitewiseAnd should manage lists of different lengths by discarding excess values.  (2 marks) 
bitwiseAnd :: [Bit] -> [Bit] -> [Bit]
bitwiseAnd = undefined

use zipWith from the prelude to apply myAnd to each element of the two lists

-}

bitwiseAnd :: [Bit] -> [Bit] -> [Bit]
bitwiseAnd = zipWith myAnd

{-
6c) Define bin2Int which converts a binary number, represented as a list of bits, to an integer. You must consider the place value of each Bit with the first element being the most significant and the last being the least significant. (3 marks)
    bit2Int [O,I] = 1
    bit2Int [I,O,I] = 5 
    bit2Int [I,I,I] = 7 
    bit2Int [I,O,O,O] = 8
bit2Int :: [Bit] -> Int 
bit2Int = undefined

foldl apply left to right 
\acc x -> acc * 2 + bitValue x is the folding function acc is the accumulator and x is the current Bit

Each step multiplies the accumulator by 2 adds the current bit integer value (1 for I and 0 for O)
bitValue helper function converts Bit to integer value
-}


bit2Int :: [Bit] -> Int
bit2Int = foldl (\acc x -> acc * 2 + bitValue x) 0
  where
    bitValue I = 1
    bitValue O = 0



{-
7) Define substrings which, for a given string, returns every possible subset of the characters within a string. Your function should preserve the ordering of characters. substrings "cat" == ["cat","ca","ct","c","at","a","t",""] (the order of the result doesn't matter) (4 marks)
substrings :: String -> [String]
substrings = undefined 

Use recursion split string into its head and tail
Then use list comprehension to create a list of all possible substrings


-}



substrings :: String -> [String]
substrings "" = [""]
substrings (x:xs) = [x:sub | sub <- substrings xs] ++ substrings xs


{-
8) Consider the following algorithm for reversing the digits of an integer.

 fun(x) {
    y = 0;
    while(x>0){
        z = x `mod` 10;
        y = y*10 + z;
        x = x `div` 10;
    }
    return y;
 }
 Implement a recursive version of this algorithm. Note that ending zeros will be dropped (1230 -> 321), this is fine. (3 marks)
 
 revDig :: Int -> Int
revDig = undefined 
 -}




revDig :: Int -> Int
revDig x = revDigHelper x 0
  where
    revDigHelper 0 y = y
    revDigHelper x y = let z = x `mod` 10
                           newY = y * 10 + z
                           newX = x `div` 10
                       in revDigHelper newX newY

testDivHel x = revDig x == (read.reverse.show) x -- this will test all positive values of your solution.

{-
Here is a data structure for leaf-labelled ternary trees:
-}

data TTree a = Node (TTree a) (TTree a) (TTree a) | Leaf a
  deriving (Show)

{-
9) Write a function that returns the largest value stored in a ternary tree (3 marks)

largestValue (Node (Leaf 4) (Leaf 5) (Leaf 6)) == 6
largestValue (Node (Leaf 19) (Leaf 5) (Leaf 6)) == 19


largestValue :: (Ord a) => TTree a -> a
largestValue = undefined

if node recursively check the largest value of the left, middle and right nodes


-}

largestValue :: (Ord a) => TTree a -> a
largestValue (Leaf a) = a
largestValue (Node l m r) = maximum [largestValue l, largestValue m, largestValue r]


{-
Here is a data structure for key-value dictionaries with integer keys:
-}

newtype Dictionary k v = Dictionary [(k, v)] deriving Show

{-
10a) A dictionary is valid if it does not contain the same key twice. Write a function that checks whether a dictionary is valid (3 marks) 

dictionaryValid (Dictionary [(1,True),(2,False),(3,False)]) == True
dictionaryValid (Dictionary [(1,True),(2,False),(3,False),(1,False)] == False

dictionaryValid :: (Eq k) => Dictionary k v -> Bool
dictionaryValid = undefined


-}

dictionaryValid :: (Eq k) => Dictionary k v -> Bool
dictionaryValid (Dictionary []) = True
dictionaryValid (Dictionary ((k,v):kvs)) = notElem k (map fst kvs) && dictionaryValid (Dictionary kvs)

-- Helper function to extract the list of keys from the dictionary for checking duplicates
keys :: [(k, v)] -> [k]
keys = map fst

{-
10b) Write a function to insert a key-value pair into a dictionary, which fails if the given key is already used in the dictionary (2 marks) 
dictionaryValid (Dictionary [(1,True),(2,False),(3,False)]) 1 True == Nothing
dictionaryValid (Dictionary [(1,True),(2,False),(3,False)]) 4 False == Just (Dictionary [(1,True),(2,False),(3,False),(4,False)])

dictionaryInsert :: (Eq k) => Dictionary k v -> k -> v -> Maybe (Dictionary k v)
dictionaryInsert = undefined

-}


-- Function to check if a key exists in the dictionary
keyExists :: (Eq k) => k -> Dictionary k v -> Bool
keyExists key (Dictionary kvs) = any (\(k, _) -> k == key) kvs

-- Function to insert a key-value pair into the dictionary
dictionaryInsert :: (Eq k) => Dictionary k v -> k -> v -> Maybe (Dictionary k v)
dictionaryInsert dict@(Dictionary kvs) key value
  | keyExists key dict = Nothing
  | otherwise = Just (Dictionary ((key, value):kvs))

{-
10c) Write a Foldable instance for dictionaries (3 marks)
-}

-- data Foldable (Dictionary k) where
--   foldr = undefined

instance Foldable (Dictionary k) where
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f z (Dictionary kvs) = foldr (\(_, v) acc -> f v acc) z kvs
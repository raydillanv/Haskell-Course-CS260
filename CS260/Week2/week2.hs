{-
Labs thursday 9-11, friday 1-3, except week 3 when it is 2-4 both usually LT1201

[1,2,3,4,5]
ghci> [1..5] == [1,2,3,4,5]

[x | x<- [1,2,3,4,5]]
== [2,3,4,5,6]

let xs = [1,2,3,4,5]
[x+1 | x <- xs]
== [2,3,4,5,6]

[x+1 | x <- [1..5]]
== [2,3,4,5,6]

pairs (1,2) in a list everythin needs to be the same type,

[(x,y) | x <- [1,2,3], y <- ["Hello", "World"]]

[x | x <- [1..10], even x]
== [2,4,6,8,10]

[1..10] !!5
== 6

"Hello Workd" !! 6
== 'W'

[] empty
: cons 
3 : [] 
== [3]

2 : (3:[])
== [2,3]

1 : (2 : (3 : []))
== [1,2,3]

'l' : ('d' : [])
== "ld"

3 : []
== [3]

3:4
== error:

[1,2,3] ++ [4,5]
== [1,2,3,4,5]

"hello" ++ "world"
== "helloworld"

head [1,2,3]
== 1

tail [1,2,3]
== [2,3]

last [1,2,3]
== 3
xs = [1,2,3,4,5]
head xs : tail xs
== [1,2,3,4,5]

You cannot do head or tail on an empty list []




head [1..10]
-}


add:: Int -> Int -> Int
add x y = x + y

myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

{-
Define encorde which takes a String and returns a list of tuples containing a character and the it's
number of occurences in a string 
e.g. encode "meooo" = [('m',1),('e',1),('o',3)]
-}

encode :: String -> [(Char, Int)]
encode [] = []
encode (x:xs) = (x, countChar x (x:xs)) : encode (removeChar x xs)


countChar :: Char -> String -> Int
countChar _ [] = 0
countChar c (x:xs) | c == x = 1 + countChar c xs
                   | otherwise = countChar c xs

removeChar :: Char -> String -> String
removeChar _ [] = []
remoceChar c (x:xs) | c == x = removeChar c xs
                    | otherwise = x : removeChar c xs
    
    


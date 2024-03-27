mult5 :: Int -> Int
mult5 x = 5*x

mux :: Bool -> Bool -> Bool -> Bool
--mux c x y = (not c) && x || c && y

--Recursion 
--mux c x y = if c then x else y
mux False x y = x
mux True  x y = y 

fact :: Int -> Int
fact 0 = 1
--fact n = n * fact (n-1)
fact n | (n < 0) = ERROR "negative number"
       | otherwise = n * fact (n-1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

i = i + 1

fizzBuzz :: Int -> String
fizz x | x 'mod' 3 == 0 = "Fizz"
       | x 'mod' 5 == 0 = "Buzz"
       | otherwise = show x

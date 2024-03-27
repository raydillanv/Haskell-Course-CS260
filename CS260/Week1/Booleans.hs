{- So far we have only seen the integer type. Of course Haskell supports an incredible number of types
(Bools, Floats, doubles, etc) and one of Haskells defining features is the wease with which we 
can define our own types. We also have the build in operators you would expect;
- &&:: Bool -> Bool _> Bool
- ||:: Bool -> Bool -> Bool
- not:: Bool -> Bool
And all the other operators you would expect (>, < >= etc) -}

mux::Bool -> Bool -> Bool -> Bool
mux c x y = (not c) && x || (c && y)

moreThan5 x = x > 5 -- Better than if statements

divBy5::Int -> Bool
divBy5 x = x `mod` 5 == 0


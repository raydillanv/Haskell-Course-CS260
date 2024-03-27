{- A String is really a list of Characters. The type String is the same as [Char], a list of characters. Strings from Haskells view:
Char1:Char2:Char3:....:Char[] -}

length1::String -> Int
length1 [] = 0 -- Base case
length1 (x:xs) = 1 + length1 xs -- Recursive case
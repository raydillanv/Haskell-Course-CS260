{-  Public String checkChar(Char x){
    if (x == 'a'){
        return "the char is A";
    } else if (x == 'z'){
        return "the char is z";
    } else {
        return "the char is not a or z";
    }
-}

checkChar :: Char -> String
checkChar x = if x == 'a' then "the char is A" else (if x == 'z' then "the char is z" else "the char is not a or z")
-- if not used very often in Haskell
-- Guards: Cleaner way to write if statements in Haskell, you can add more guards
checkChar1::Char -> String
checkChar1 x | x == 'a' = "the char is A"
             | x == 'z' = "the char is z"
             | otherwise = "the char is not a or z"

-- Pattern Matching: Cleaner way to write if statements in Haskell, you can add more guards
checkChar2::Char -> String
checkChar2 'a' = "the char is A"
checkChar2 'z' = "the char is z"
checkChar2  _  = "the char is not a or z"

--Often more ways than 1 way of writing code, the above 2 are preferable as they are cleaner

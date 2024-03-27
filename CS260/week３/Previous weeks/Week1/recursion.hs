{-function gcd(a, b)
    while a != b
        if a > b
            a := a - b
        else
            b := b - a
    return a-}

-- Takes two integers returns another
gcd1:: Int-> Int-> Int
gcd1 a b | a == b = a
        | a > b = gcd1 (a-b) b
        | otherwise = gcd1 a (b-a)
-- We update argueent because we can't mutate variables in Haskell
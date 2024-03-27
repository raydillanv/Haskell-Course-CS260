{- Imperative vs Declarative 
Haskell is a declarative language which means that you program in terms of what you want, not how you get it.

public int summ(int n) {
        private int k,j:
        k=0; j=1
        while(j<n+1){
            k=k+j;
            j=j+1;
        }
        return k;
}

                -}

-- Run the following in terminal stack ghci src/(Your Hs file)

summ::Int -> Int
summ 0 = 0 
summ n = n + summ (n-1)


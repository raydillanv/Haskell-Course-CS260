{-
data State s a = State (s -> (a, s))
    deriving (Functor, Applicative)

returnState :: a -> State s a
returnState x = State (\s -> (s, x))

blindState :: State s a 
            -> (a -> State s b)
            -> State s b
blindState (state f) g
    = State $ \s1 -> let (s2, x) = f s1
                        State h = g x
                    in h s2


instance Monad (State s) where
  return a = State (\s -> (a, s))
  (State x) >>= f = State (\s -> let (a, s') = x s in runState (f a) s')
-}


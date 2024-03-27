data Suit = Diamonds | Clubs | Hearts | Spades 
   deriving(Eq, Show, Ord, Enum) -- Enum allows us to use the range notation [..]


data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
     deriving (Eq,Show,Ord,Enum)


data Card = MkCard Rank Suit 
  deriving(Eq,Ord,Show)

type Deck = [Card]

getSuit :: Card -> Suit
getSuit (MkCard x y) = y 

makeDeck :: Deck
makeDeck = [MkCard x y | x <- [Two ..Ace], y <- [Diamonds ..Spades] ]

data Nullable a = Null | NotNull a
  deriving (Show,Eq,Ord)

newHead :: [a] -> Nullable a
newHead [] = Null
newHead (x:xs) = NotNull x

data List a = Empty | Cons a (List a)
  deriving (Show,Eq)

testList = Cons "Clemens" (Cons "Bob" (Cons "Fred" (Cons "Neil" (Cons "Conor" Empty))))

myLength :: List a -> Int
myLength Empty = 0
myLength (Cons x xs) = 1 + myLength xs

myMap :: (a -> b) -> List a -> List b
myMap f Empty = Empty
myMap f (Cons x xs) = Cons (f x) (myMap f xs)


data BTree a = Leaf | Node (BTree a) a (BTree a)
 deriving(Eq,Show)

testTree :: BTree Int
testTree = Node (Node (Node Leaf 
                            1 
                            Leaf) 
                       3 (Node Leaf 
                               4 
                               Leaf)) 
                5 
                (Node (Node Leaf 6 Leaf) 7 Leaf)

test2 :: BTree Int
test2 = Node Leaf 5 Leaf

sumTree :: BTree Int -> Int 
sumTree Leaf = 0 
sumTree (Node lt x rt) = (sumTree lt) + x + (sumTree rt)

insertTree :: (Ord a) => a -> BTree a -> BTree a
insertTree x Leaf = Node Leaf x Leaf
insertTree x (Node lt y rt) 
  | (x <= y) = Node (insertTree x lt) y rt
  | otherwise = Node lt y (insertTree x rt)
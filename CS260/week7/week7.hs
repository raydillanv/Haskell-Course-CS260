{-# LANGUAGE DeriveAnyClass #-}

staff :: [(String, Int,[String])]
staff = [("Jules Hedges", 211, ["CS260","CS825"]),
         ("Alasdair Lambert",121,["CS106", "CS107","CS260"]),
         ("Joe Bloggs",457,["CS501","CS123","CS312"]),
         ("Sarah Smith",876,["MM101","MM409"]),
         ("Mark Markson",404,[]),
         ("Ada Lovelace",209,["MM312","MM401"]),
         ("Joe Johnson",999,["EC209"]),
         ("Clemens Kupke",412,["CS209"])]

classes :: [(String, [String])]
classes = [("Computer and Information Sciences",["CS106","CS107","CS103","CS260","CS825"]),
           ("Mathematics and Statistics",["MM101","MM203","MM409","MM312","MM401"]), 
           ("Economics",["EC209","EC104"])]

faculties :: [(String, [String])]
faculties = [("Science", ["Computer and Information Sciences","Mathematics and Statistics"]),
             ("Business", ["Economics", "Finance"])]



getClasses :: String -> Either MyError [String] 
getClasses x = foldr (\(n,g,l) acc -> if x == n then Right l else acc) (Left LecturerNotFound) staff
 --Foldr checks to see if it can find the value x in the list of staff (the staff member name) in the list of staff 
 --Left LecturerNotFound is the exception return 

isIn :: [String] -> [String] -> Bool -- checks if any element of xs is in ys 
isIn [] _ = False 
isIn (x:xs) ys | x `elem` ys = True  -- if one element is then okay
               | otherwise = isIn xs (y:ys) -- else check the next element

getDept :: [String] -> Either MyError String
getDept xs = foldr (\(n,cs) acc -> if xs `isIn` cs then Right n else acc) (Left DeptNotFound) classes
-- searches the classes list and tries to return the name of the department, the Left value again represents failure

getFaculty :: String -> Either MyError String 
getFaculty x = foldr (\(n,cs) acc -> if x `elem` cs then Right n else acc) (Left FacultyNotFound) faculties 
--tries to find the corresponding faculty for a department by checking is the given string is in the list of departments


stick :: Maybe a -> (a -> Maybe b) -> Maybe b -- this is the type of (>>=) pronounced bind 
stick Nothing f = Nothing -- if we have a Nothing we can't apply f!  
stick (Just x) f = (f x)  -- if we have just X we throw away the just and return (f x)

myReturn :: a -> Maybe a -- injects a value of a into Maybe a 
myReturn x = Just x 



data MyError = LecturerNotFound | DeptNotFound | FacultyNotFound -- Either lets us return a custom error message rather than Nothing
    deriving Show

data State s a = State (s -> (s, a))
  deriving (Functor, Applicative)

returnState :: a -> State s a
returnState x = State (\s -> (s, x))

bindState :: State s a 
          -> (a -> State s b) 
          -> State s b
bindState (State f) g
  = State $ \s1 -> let (s2, x) = f s1
                       State h = g x
                    in h s2
  
instance Monad (State s) where
  return = returnState
  (>>=) = bindState

data MyState = S1 | S2 deriving (Show)
data Alphabet = A | B deriving (Show)

stateAccepts :: MyState -> Bool
stateAccepts S1 = False
stateAccepts S2 = True

readLetter :: Alphabet -> State MyState ()
readLetter x = State $ \s ->
    case (x, s) of
        (A, S1) -> (S2, ())
        (B, S1) -> (S1, ())
        (A, S2) -> (S1, ())
        (B, S2) -> (S2, ())

readString :: [Alphabet] -> State MyState ()
readString [] = return ()
readString (x:xs) = readLetter x >> readString xs

acceptString :: [Alphabet] -> Bool
acceptString xs = let State f = readString xs
                      (finalState, ()) = f S1
                   in stateAccepts finalState

f :: Int -> [Int]
f x = [x, x + 1]

g :: Int -> [Int]
g x = if x == 4 then [4] else []


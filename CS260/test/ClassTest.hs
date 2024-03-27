{- -----------READ BEFORE ATTEMPTING -------------------------------------}

{-If the file you submit does not compile, your mark will be capped at 50%. You can test your code by using :l before submitting.

If you want to submit an answer for partial marks that you know doesn't compile, please comment it out.

Make sure to submit your solutions by 5:00pm, late submissions will not be marked unless a valid reason is given. 

While the test is open book all solutions must be entirely your own work. Any suspected plagiarism will be investigated in line with the university's academic dishonesty policy. 

You may define helper functions and use any function from the Haskell prelude and the given imported libraries. Here are the docs for the libraries you have available:

https://hackage.haskell.org/package/base-4.19.1.0/docs/Prelude.html
https://hackage.haskell.org/package/base-4.19.1.0/docs/Data-List.html
https://hackage.haskell.org/package/base-4.19.1.0/docs/System-IO.html
https://hackage.haskell.org/package/QuickCheck-2.14.3/docs/Test-QuickCheck.html

You may not import any other libraries, if you do then your mark will be capped at 50%. IMPORTANT: Please note that some tools, including Haskell Language Server, can automatically add imports. Double check your import list before submission!

Questions are not ordered by difficulty. 

In this test you will be developing a small database application. We have provided you with a csv file, Films.csv. There are 4 columns: (1) A unique identifier, an integer, (2) The name of the film with whitespace removed, (3) RottenTomatoes score, an integer between 0 and 100 (inclusive), and (4) whether or not the film contains ninjas. None of the questions will ask you to write to the CSV file, so you don't need to worry about corrupting it. Download this csv to complete the questions below. 
-}

module ClassTest where

{- You may use any function from the following 3 libraries, but no others.
You may also comment out any of these imports if you don't want to use them
(all questions can be solved using only functions in Prelude) -}
import Data.List
import System.IO
--import Test.QuickCheck

-- Here is a datatype representing a row of the database:

data Row = Row Int String Int Bool
  deriving (Show, Eq)

-- Q1: Write a function that will check whether the given database (a list of Rows) is valid, ie. does not contain the same key twice, and only contains scores within the valid range. [2 marks]

{- Note, I learned Haskell 2 years ago at my US university and refreshed with this guys videos, which i highly reccomend:
https://www.youtube.com/watch?v=-DHEmrKhjCM&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&index=43&ab_channel=PhilippHagenlocher
-}

--databaseValid :: [Row] -> Bool
--databaseValid = undefined

-- Here is a datatype representing all of the exceptions that might be encountered anywhere in the test. Several questions below have output types of the form Either Exception <something>, and for each one you should choose the appropriate exception to return in case of a failure.

data Exception = UIDAlreadyPresentException
               | UIDNotPresentException
               | TooManyOrNotEnoughNinjasException
               | CSVParseException
               | CommandParseException
  deriving (Show)

-- Helper func to extract keys from the database rows
extractKeys :: [Row] -> [Int]
extractKeys = map (\(Row key _ _ _) -> key)

-- Helper func to check for duplicate keys
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = length xs /= length (nub xs)

-- Helper func to check if a score is valid
validScore :: Int -> Bool
validScore score = score >= 0 && score <= 100

-- Check if all scores are valid
allScoresValid :: [Row] -> Bool
allScoresValid = all (\(Row _ _ score _) -> validScore score)

-- Main validation func
databaseValid :: [Row] -> Bool
databaseValid rows = not (hasDuplicates (extractKeys rows)) && allScoresValid rows


-- Q2: Write a function that will insert a row into the database, throwing an exception if the UID is already present. You may insert the new row anywhere in the current database if it is valid. [2 marks]

--insert :: Row -> [Row] -> Either Exception [Row]
--insert = undefined

-- Checks if a UID is already present in the database
uidPresent :: Int -> [Row] -> Bool
uidPresent uid rows = uid `elem` extractKeys rows

-- Insert a row into the database
insert :: Row -> [Row] -> Either Exception [Row]
insert newRow@(Row uid _ _ _) rows
  | uidPresent uid rows = Left UIDAlreadyPresentException
  | otherwise = Right (newRow : rows)

-- Q3: Write a function that will delete a row with the given UID from the database, throwing an exception if the UID is not present. [2 marks]

--delete :: Int -> [Row] -> Either Exception [Row]
--delete = undefined

delete :: Int -> [Row] -> Either Exception [Row]
delete uid rows
  | not (uidPresent uid rows) = Left UIDNotPresentException
  | otherwise = Right (filter (\(Row key _ _ _) -> key /= uid) rows)


-- Q4: Write a function that will compute the pair of the average score of all the films that contain ninjas, and the average score of all the films that don't contain ninjas. If either of the averages is not mathematically well-defined, you should throw an exception. [4 marks]

--averageScores :: [Row] -> Either Exception (Double, Double)
--averageScores = undefined

averageScores :: [Row] -> Either Exception (Double, Double)
averageScores rows = let
    ninjaFilms = [score | (Row _ _ score True) <- rows]
    nonNinjaFilms = [score | (Row _ _ score False) <- rows]
    avg lst = realToFrac (sum lst) / fromIntegral (length lst)
  in case (ninjaFilms, nonNinjaFilms) of
    ([], _) -> Left TooManyOrNotEnoughNinjasException
    (_, []) -> Left TooManyOrNotEnoughNinjasException
    _ -> Right (avg ninjaFilms, avg nonNinjaFilms)


-- Q5: Write a function that, given a string containing the raw contents of a CSV file, parses the contents of the Filsm.csv file into a list of rows, throwing an exception if the file is not correctly formed. You can assume the raw contents will be produced via the readFile function. [4 marks]

--parseCSV :: String -> Either Exception [Row]
--parseCSV = undefined

-- Here is a datatype representing a small language of boolean conditions about rows

data Condition = NameEquals String
               | ScoreGreaterThan Int
               | ScoreLessThan Int
               | HasNinjas
  deriving (Show)

-- Utility func to trim spaces from a string (not part of Prelude)
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile (== ' ')

-- Parses a single line into a Row
parseRow :: String -> Either Exception Row
parseRow str = case wordsWhen (==',') str of
    [uidStr, name, scoreStr, ninjaStr] -> case (reads uidStr, reads scoreStr, parseBool ninjaStr) of
        ([(uid, "")], [(score, "")], Right hasNinja) -> Right (Row uid (trim name) score hasNinja)
        _ -> Left CSVParseException
    _ -> Left CSVParseException

-- Splits a string by a given delimiter
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
          where (w, s'') = break p s'

-- Parses a "true" or "false" string to a Bool without using toLower
parseBool :: String -> Either Exception Bool
parseBool str = case trim str of
    "true" -> Right True
    "True" -> Right True
    "false" -> Right False
    "False" -> Right False
    _ -> Left CSVParseException

-- Parses the entire CSV content
parseCSV :: String -> Either Exception [Row]
parseCSV content = mapM parseRow (lines content)


-- Q6: Write a function that, given a condition and a row, checks whether the row satisfies the condition. [4 marks]

--checkCondition :: Condition -> Row -> Bool
--checkCondition = undefined

checkCondition :: Condition -> Row -> Bool
checkCondition (NameEquals name) (Row _ rowName _ _) = rowName == name
checkCondition (ScoreGreaterThan score) (Row _ _ rowScore _) = rowScore > score
checkCondition (ScoreLessThan score) (Row _ _ rowScore _) = rowScore < score
checkCondition HasNinjas (Row _ _ _ hasNinja) = hasNinja

-- Q7: Write a function that, given a condition and a database, selects the rows of the database that satisfy the condition. [4 marks]

--select :: Condition -> [Row] -> [Row]
--select = undefined

select :: Condition -> [Row] -> [Row]
select condition = filter (checkCondition condition)

-- Here is a datatype representing a simple command language of user inputs

data Command = Select Condition
             | Insert Row
             | Delete Int
             | Quit
  deriving (Show)

--Q8: Write a function that takes a list of commands and sequences them one after the other, carrying along the state of the database. If a Quit command is reached then the function should terminate and return the current state of the database. If one of the commands throws an exception then that exception should be returned. [5 marks]

--interpretCommands :: [Command] -> [Row] -> Either Exception [Row]
--interpretCommands = undefined

interpretCommands :: [Command] -> [Row] -> Either Exception [Row]
interpretCommands [] db = Right db 
interpretCommands (cmd:cmds) db = case cmd of
    Select _ -> interpretCommands cmds db  
    Insert row -> case ClassTest.insert row db of  -- try to insert a row
        Left exception -> Left exception 
        Right newDb -> interpretCommands cmds newDb
    Delete uid -> case ClassTest.delete uid db of  -- try to delete a row by UID
        Left exception -> Left exception  
        Right newDb -> interpretCommands cmds newDb
    Quit -> Right db  -- if quit then quit bro

{- Q9: Write a function that will parse a raw string of user input into a command, or throw an exception if the user input is not correctly formed. The syntax of the user input is given by the following grammar:
  Command ::= Select [NameEquals s | ScoreGreaterThan n | ScoreLessThan n | ContainsNinjas] 
            | Insert n s n [Yes | No]
            | Delete n
            | Quit
where n means any integer (you may assume it is positive, ie. does not contain a leading '-' character) and s is any alphanumeric string (you may assume it does not contain any whitespace character). Examples of strings which should successfully parse are "Select ScoreGreaterThan 5" and "Insert 23 HouseOnHauntedHill 79 No"
[4 marks] -}

--parseCommand :: String -> Either Exception Command
--parseCommand = undefined

parseCommand :: String -> Either Exception Command
parseCommand input = let
    parts = words input
  in case parts of
    ("Select":condParts) -> parseSelectCondition condParts
    ("Insert":rowParts) -> parseInsertRow rowParts
    ("Delete":uidStr:[]) -> case reads uidStr of
                              [(uid, "")] -> Right $ Delete uid
                              _ -> Left CommandParseException
    ("Quit":[]) -> Right Quit
    _ -> Left CommandParseException

parseSelectCondition :: [String] -> Either Exception Command
parseSelectCondition parts = case parts of
    ["NameEquals", name] -> Right $ Select $ NameEquals name
    ["ScoreGreaterThan", nStr] -> case reads nStr of
        [(n, "")] -> Right $ Select $ ScoreGreaterThan n
        _ -> Left CommandParseException
    ["ScoreLessThan", nStr] -> case reads nStr of
        [(n, "")] -> Right $ Select $ ScoreLessThan n
        _ -> Left CommandParseException
    ["HasNinjas"] -> Right $ Select HasNinjas
    _ -> Left CommandParseException

parseInsertRow :: [String] -> Either Exception Command
parseInsertRow [uidStr, name, scoreStr, ninjaStr] = case (reads uidStr, reads scoreStr, parseNinja ninjaStr) of
    ([(uid, "")], [(score, "")], Just hasNinja) -> Right $ Insert $ Row uid name score hasNinja
    _ -> Left CommandParseException
  where
    parseNinja "Yes" = Just True
    parseNinja "No" = Just False
    parseNinja _ = Nothing
parseInsertRow _ = Left CommandParseException


-- Q10: Write a function that is an exception handler that will "wrap" any given function from values to IO actions. If the input is an ordinary value then it should pass it to the function and perform the IO action, whereas if the input is an exception then it should print the string representation of the exception (the one that the show function generates) to the terminal. [4 marks]

--handle :: (a -> b -> IO b) -> Either Exception a -> IO b
--handle = undefined

handle :: Show e => (a -> IO b) -> Either e a -> IO ()
handle _ (Left e) = print e
handle f (Right a) = f a >> return ()


-- Q11: Write a main function implementing a read-eval-print loop for the command language. On each loop it should print the prompt string "> " with no newline, and wait for user input. After receiving user input it should parse it and execute the command, catching and printing exceptions. If a quit command was received the function should terminate, otherwise the user should be prompted for input again. [5 marks]

--main :: IO ()
--main = undefined

main :: IO ()
main = repl []

repl :: [Row] -> IO ()
repl db = do
  putStr "> "
  hFlush stdout -- display prompt
  input <- getLine -- Read user input
  case parseCommand input of
    Left exception -> print exception >> repl db -- If parsing fails, print error and continue.
    Right Quit -> putStrLn "Quitting..." -- if quit then quit loop bro 
    Right command -> 
      -- handle command types
      case command of
        Select condition ->
          -- print matching rows
          (print $ select condition db) >> repl db
        Insert row ->
          -- local insert function
          case ClassTest.insert row db of
            Left exception -> print exception >> repl db
            Right newDb -> repl newDb
        Delete uid ->
          -- local delete function
          case ClassTest.delete uid db of
            Left exception -> print exception >> repl db
            Right newDb -> repl newDb

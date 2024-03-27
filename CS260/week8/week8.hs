import Data.List.Split

helloWorld = putStrLn "Hello World!" --performs an i/o operation, printing to the terminal

helloWorld2 = putStrLn "What is your name?" -- >> let's us perform an effectful computation, in this case an I/O operation and then carry on to the next
           >> getLine >>= (\name -> --the bind then lets us pipe the output of getline into putStrLn The lambda lets us name the output of bind to feed it into the next
              putStrLn  ("Hello, " ++ name))

helloWorld3 = do putStrLn "What is your name?" -- do notation abstracts away >> and >>= for any monad
                 name <- getLine -- <- uses bind and assigns the output to the variable name
                 putStrLn ("Hello, " ++ name)

thingy = do classes <- getClasses "Jules Hedges"
            getDept classes
-- tingyDesugared shows how the compiler converts the do notation to >>= and >>            
thingyDesugared = getClasses "Jules Hedges" >>= (\classes ->
                  getDept classes)

tidy :: String -> [(Int, String)]
tidy xs = map (\(x,y) -> (read x,y)) -- convert type of first element to Int
        $ map (\[x,y] -> (x,y)) --change type from lists of 2 elements to pairs
        $ map (splitOn ",") (words xs)--split on whitespace/newlines, gives a list of strings, then use map to split the elements into lists containing a key and value

readInFile :: FilePath -> IO [(Int, String)]
readInFile filename = do rawFile <- readFile filename --input given file, converts to string
                         return (tidy rawFile) -- return the version parsed by tidy

whatDoesThisDo :: IO Int
whatDoesThisDo = do return 3
                    return 4
                    return 5 -- return does not give back control from the function, it injects values into the monad type

withFile key = do database <- readInFile "test.csv"
                  value <- return $ (lookup key database) -- get the value we're interested in
                  case value of -- case expression allows us to drop into pattern matching during the function
                    Nothing -> do putStrLn "Not found" --we need do here to get us back into do notation
                    Just name -> do putStrLn ("The name you found is: " ++ name)

import Data.List
import Data.Char 
--Getting Started
--------------------------------------------------

--Quick note, alt+z will turn on/off word wrap.

{- These introductory notes will hopefully help to reinforce the first weeks materials. Feel free to load this file into GHCI and test the functions, you can change the definitions to see what works and what doesn't. This file is a bit longer than usual! there are some things in this file you may want to return to, we will also get much more experience as we go along.

Imperative vs Declarative 
---------------------------------------------------

Haskell is a declarative language. In declarative languages we tell the computer what to do rather than how to go about doing it, this means we worry much less about managing and updating local variables (in fact you can't mutate state in Haskell without using some fancy tricks we will learn later). In imperative languages we use things like loops to check and update values and produce results, in declarative languages we tell the computer what it should do and let it worry about state and memory management. As a rough rule of thumb, if a language has explicit loops then it is imperative.

Haskell is a functional language, this is a form of declarative programming. There is no one consensus on what makes a language functional but in Haskell we will build programs by defining and composing functions. 

Basic Syntax
---------------------------------------------------

Here is a simple Haskell function that takes a number and returns 2 times that number -} 

twice:: Int -> Int  --type signature
twice x = 2*x       --function definition

{- Often simple functions in Haskell will be similar to functions you are used to from school maths e.g. f(x) = 2*x. On the left hand side we have the name of the function as well as the arguments it takes, on the right we have the actual definition of function behaviour. Note that in Haskell we drop the brackets. 

Going back to twice, now that we have defined the function twice we can reuse it to build other functions. -}

timesFour:: Int -> Int
timesFour x = twice (twice x)  

{-Note here the use of brackets to group twice x as one argument. By building and chaining functions together we can build substantial complexity quickly with sections that are easy to debug. 

In fact we can use variables in Haskell but not in the way you'd expect, as Haskell is a pure language we cannot change the state of a variable. Try copying the following code out of this comment and loading into ghci.

n = 5
x = 9 
n = 6

Also, this line of code is valid in Haskell but it's not what you expect! try running it so see what it does.
-}

i = i + 1

{-
One thing we haven't mentioned so far are types. In Java we need to declare the return type of the method as well as the type of the arguments, similarly we need to decare these in Haskell. Haskell can infer the types of functions but it is generally best to declare them.
 -}
--     input  input  return
times:: Int -> Int -> Int 
times x y = x * y 

{- This top line is known as the type signature or type declaration. You will be expected to give the signature of every function you declare unless told otherwise.

Haskell has an incredibly well developed type system, in fact one of the key benefits of Haskell is the ability to define our own types. Of course we have many types at our disposal including Bools (Booleans), Strings, Floats etc. Note that types start with capitals, this is a reserved syntax in Haskell. -}

xor:: Bool -> Bool -> Bool               -- takes two true or false values and returns true or  false
xor x y = (not x && y) || ( x && not y)  -- simple xor using built in logical operators 

-- try using xor in terminal, you'll need to use True and False as arguments

-- Note the double && and ||, the single versions are reserved for other notations / functions.

mux:: Bool -> Bool -> Bool -> Bool
mux c x y = (not c && x) || (c && y) 

--You may notice this code is extremely similar to Syrup, this is no accident! 

{-Let's look at a more involved example. Here is some java pseudocode for finding the sum of all numbers up to the one given 

Public Int sumN(int n){
  int total = 0;
  for(int i = 1; i<=n; i++){
     total = total + i;
  }
     return total;
}

In Java we work by updating the state of variables and objects. In Haskell this code becomes
-} 

sumN::Int -> Int        --one Int as argument and one as return type
sumN 0 = 0              --base case. This style is known as pattern matching, this is where we match behaviour directly to input values. 
sumN n = n + sumN (n-1) --recursively move towards zero

{- Here we use recursion along with a base case to carry out the same calculation, there is importantly no for loop. Note that the code is much more concise than the equivalent java code. We can also use multiple lines to define the function for different cases.  
-}

-- you could also define sumN using an if but it's a bit less clean! 
sumN2::Int->Int
sumN2 n = if n==0 then 0 else n + sumN2 (n-1) 

--Or using guards
sumN3:: Int -> Int 
sumN3 n | n == 0     = 0
        | otherwise  = n + sumN3 (n-1)

{-Guards are somewhat similar to if statements. They are of the form | <predicate> = <behaviour>, where the predicate must return a boolean. If the boolean returns True the the associated behaviour is carried out. You can use multiple guards in a function definition. The otherwise keyword functions much like else where if none of the previous cases are met the otherwise condition will be executed.
        
These are equivelant and often there will be many ways to define functions. Try running these in GHCI to see that they are equivalent.

Note that we don't need to define temporary variables! We work only with the function and it's argument.

In imperative languages we often rely on nested if statements to apply logical conditions. Indeed we can do this in Haskell. Lets define our own absolute value function. We'll use three cases though it could be easily defined with 2. -}

abs1:: Int -> Int
abs1 x = if (x == 0) then 0 else (if x>0 then x else (-x))

{-clearly this is fine for simple cases but as we get more complex these ifs become harder to read and understand. Remeber that guards allow us to define behaviour for conditions that evaluate to a boolean value.-}

abs2:: Int -> Int
abs2 x | x == 0    = 0    
       | x >  0    = x
       | otherwise = (-x) 
       
{-often guards are clearer to read as we can clearly see each condition without worrying about keywords. Note that these conditions are checked in sequence, so Haskell checks if a number is zero, then if it's positive and finally the otherwise case so be careful to list them in the right order. One thing about guards which is nice is that we can combine them with pattern matching on input. -}

abs3::Int -> Int
abs3 0 = 0
abs3 x | x > 0     = x
       | otherwise = (-x) 

{- this gives us a nice neat definition. It is also nice that we can easily extend conditions to include more guards if needed which is more cumbersome with ifs. All of these functions are the same in practice, in general in Haskell there will be many ways to define the same behaviour so don't worry if your solution isn't exactly the same as someone elses. Note that often ifs are used in Haskell in input/output operations-}

{-
In Haskell we can respresent lists as types. One interesting thing to note is a list of things in Haskell e.g [1,2,3,4] is actually a bunch of single objects stuck together as 1:2:3:4:[]. : is called cons, it sticks objects of the right type onto the front of a list. This means we can write methods on lists by treating each element individually. Strings are types of lists, they are lists of Chars! In Haskell String = [Char] where the square brackets tell us that the type is a list. Lets write a function to find the length of string. -}

length1:: String -> Int
length1  []    = 0                 -- the base case is when we reach the end of the string i.e the empty list
length1 (x:xs) = 1 + (length1 xs)  -- count each non-empty element by adding 1

{- Here we iterate through each element, if it's not the empty list we add to the counter. This function is actually not specific to Strings, in fact we can change the types for another type of list-}

--        [] around a type mean a list of that type, [Int] = list of integers
length2:: [Int] -> Int
length2   []   = 0
length2 (x:xs) = 1 + (length2 xs)


{- Some tips for beginning
----------------------------------------------
     - When defining recursive functions write the base case first. Haskell may find another case which the base matches and do that instead. Try changing the order of cases in sumN and seeing what the outcome is.
     - No tabs! Tabs can cause errors in Haskell which may not be so easy to spot, stick to spaces.
     - Types are your friends and can make your life much easier when you get used to them. Try to make sure you understand the types of the functions you are dealing with.
     - Try to reason by case. Think of the cases your function can encounter, earlier we looked at the length function. There we thought about what to do when the list is empty and then what to do when it is not. Try to break functions down into cases you can easily manage.
     - Remember we don't always need variables, we can work with only the input and output of functions in most cases.
     - Just like in java if a function resolves to the type you need you can just evaluate that function. For example foo x = x > 5 is better than foo x = if x>5 then True else False.
     - ctrl+c will kill any infinite processes, you'll likely need this at some point in your Haskell career!
     - Haskell is known for its clean code, try to keep your function definitions as short as possible. 
-}


{-Useful Commands in GHCI
---------------------------------------------
The following commands will help you while using GHCI.
- :l (file.hs), loads the given file into GHCI. Note you will need to navigate into the correct folder while in terminal.
- :r , reloads the last file loaded in.
- :t (function name), returns the type of the function supplied.
- :i (function/typeclass/module), provides more information than :t. While :t returns only the type :i returns the type, constraints, typeclasses, location of the definition and more. It is best to stick to :t when working with functions, :i is best used on typeclasses which we will see later.
- :q, quit Haskell and return to terminal.
- :?, lists all commands available
- :! (bash command), allows you to execute terminal commands while in Haskell. For example, :! clear will clear the terminal window.
 -}
foo ::(Eq a,Num a) => a -> a -> a
foo x y | x == y = x * y
             | otherwise = x + y

foo2:: Int -> Int -> Int
foo2 x y = x + y

function:: Int -> Int
function x | x < 0 = 1
                   | otherwise = x * function (x-3)
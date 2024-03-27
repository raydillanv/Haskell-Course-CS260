{-Polymorphism and Typeclasses 
---------------------------------------------------------
Last week we saw that  we could write the following two functions to return the length of a list. -}

length1::String-> Int 
length1 [] = 0 
length1 (x:xs) = 1 + length1 xs

length2::[Int] -> Int 
length2 [] = 0
length2 (x:xs) = 1 + length2 xs

{-Note that the code for the two methods is exactly the same, we don't need to change how we check the length of a list despite the fact that they are working on different types. Surely it would be better to write one function for any type of list? This is where polymorphism comes in. Polymorphism can take different forms in different languages, in Haskell we will be working with parametric polymorphism. This is where we specify an abstract type when defining a function. This may sound complex but let's define length for an abstract type.  -}

lengthPoly:: [a] -> Int 
lengthPoly [] = 0
lengthPoly (x:xs) = 1 + lengthPoly xs

{-This function can now be applied to any type which can be put into a list. We leave the type abstract by specifying the list of type a for any arbitrary type. This is similar to Java generics however we don't need to write a whole class, instead we can be generic on a function by function basis.-}


{- We normally want to be as generic as we can, however this must be within reason and often you would like to constrain types with certain behaviour while retaining polymorphism. Consider the following code from last week, the type has been changed to be more generic:

check4::  a -> [a] -> Bool                 
check4 a  []    = False                    
check4 a (x:xs) | a == x    = True         
                | otherwise = check4 a xs


Try taking this code out of the comment, you'll notice that it now has an attached error message. This is because not every type can implement ==, in fact it doesn't make sense to insist that every type should. Here we reach a point where we cannot be completely generic in our types. To salvage this we turn to typeclasses, these allow us to insist that there is some constraint on the types we allow. A typeclass is somewhat like a Java interface where the types have to have an implementation of some functionality. 

Let us consider == which is part of the Eq typeclass. Clearly this makes sense on types such as Int but what about any other type? We don't know if the type we will be given can implement == so we should specify this in advance. 
 -}
check4:: Eq a => a -> [a] -> Bool                 
check4 a  []    = False                    
check4 a (x:xs) | a == x    = True         
                | otherwise = check4 a xs

{- Here Eq a => is a constraint insisting that whatever the type a is it must be able have an implementation of the Eq typeclass. You can find out more about typeclasses by calling :i (typeclass) in terminal or by searching for the typeclass in Hoogle. Another typeclass that is of interest is Num. Clearly + (and other basic arithmetic operations) should operate on more than just Int, we also have Float, Double, Integer etc. The Num typeclass includes all types that can implement these basic operations. What if we would like to implement a function which adds numbers if they are not equal but multiplies if they are the same? In this case we will need Eq to check for equality but another typeclass to make sure that we can add and multiply.   -}

add::(Eq a,Num a) => a -> a -> a --Here we can constrain a by two typeclasses
add x y | x == y = x*y 
        | otherwise = x + y 


{-The Num typeclass constrains types to those which have instantiations of +, *, /, - and a few others. When checking a typeclass in Haskell often you will see something referred to as the minimal definition, this is the least amount of functionality that Haskell needs to instatiate the typeclass. For example, in Eq the minimal defintion is == | /=. If we have a definiton of one that is enough as we can negate what we are given. 

Some useful basic typeclasses are Show, Eq, Ord, Enum, Num.

In future we will see what it means to instantiate a typeclass for a type, we will need to be able to define types for that.  -}

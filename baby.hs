doubleMe x = x + x
doubleUs x y = x + x + y + y
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z

-- Integral includes only whole numbers
lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!!!"  
lucky x = "Out of luck pal"   

sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"  --Only default at the end

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  

--addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
--addVectors a b = (fst a + fst b, snd a + snd b)  

--Better way
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) 

--For triples we need to make functions to extract components
first :: (a, b, c) -> a  
first (x, _, _) = x  

second :: (a, b, c) -> b  
second (_, y, _) = y  

third :: (a, b, c) -> c  
third (_, _, z) = z  

--Pattern matching a list!!!
-- x:xs would match head to x and rest of list to xs
-- Creating our own head function this way
head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x  --Remember _ means we do not care about the rest of the function

--Show elements from list
tell :: (Show a) => [a] -> String 
tell [] = "The list is empty"  
tell (x) = "The list has one element: " ++ show x  
tell (x:y) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

--Create length function using recursion and pattern matching
length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs  
--Can't use ++ in pattern matches!!!!!

--GUARDS (like if statements but easier to read!)
--bmiTell :: (RealFloat a) => a -> String  
--bmiTell bmi  
--    | bmi <= 18.5 = "You're underweight, you emo, you!"  
--    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
--    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
--    | otherwise   = "You're a whale, congratulations!" 

--By putting where after guards we can define ourfunctions conciseley
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0 

--Take a list of BMI's and return list of weight height pairs
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2 

--Using let
--Calculate surface area of a cylynder
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea 
--let <bindings> in <expression>
--(let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)

--Cases
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of 
                                                [] -> "empty."  
                                                [x] -> "a singleton list."   
                                                xs -> "a longer list."  

--Recursion
--Maximum function using recursion
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  --empty
maximum' [x] = x  --1 element
maximum' (x:xs)   --list split into head and tail
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs  --get the tail then recursiveley return

--Replicate function using recursion
replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x  --head + n-1 repititions

--Take function using recursion
take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  --Empty list if 0 or less n    If n is greater than 0 go to next pattern
take' _ []     = []  --If we try to take from an empty list we get an empty list  
take' n (x:xs) = x : take' (n-1) xs --Take head then run take n-1 times on the tail

--Reverse function using recursion
reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]  --Get tail and add back head recursively

--Repeat function using recursion (returns infinite list)
repeat' :: a -> [a]  
repeat' x = x:repeat' x  --Repeat the list

--Zip function using recursion ('zips' two lists together) zip [1,2,3] [2,3] returns [(1,2),(2,3)]
zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  --If we zip a list and empty list we get an empty list
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys  --We pair up the heads of the list and recursiveley zip the tail

--Elem function using recursion (Takes a list and checks if element is contained in it)
elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  -- Stop if true
    | otherwise = a `elem'` xs   -- Recursivekey run until end which will return false

--Quicksort in Haskell
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  --Empty list
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  --Numbers less than x
        biggerSorted = quicksort [a | a <- xs, a > x]  --Numbers greater than x
    in  smallerSorted ++ [x] ++ biggerSorted  --Combine lists greater and less than x
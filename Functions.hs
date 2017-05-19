import Data.Char
-- foldl :: (b -> a -> b) -> b -> [a] -> b
foldl' fu z [] = []
foldl' fu z (x:xs) = foldl fu (x fu z) xs

-- foldr :: (a -> b-> b) -> b -> [a] -> b
foldr' :: (a -> b -> b) -> b ->[a] -> b
foldr' fu z [] = []
foldr' fu z (x:xs) = x fu foldr fu z xs

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
    where  foldingFunction (x:y:ys) "*" = (y * x):ys
           foldingFunction (x:y:ys) "+" = (y + x):ys
           foldingFunction (x:y:ys) "-" = (y - x):ys
           foldingFunction xs numberString = read numberString:xs

-- curry
--curry' :: ((a,b) ->c) -> a -> b -> c
--curry f a b = f (a,b)

--uncurry
--uncurry' :: (a -> b -> c -> ((a,b) ->c))
--uncurry f (a,b) = f a b

--Recursion

--1 Maximum of a list
max' :: (Ord a) => [a] -> a
max' [] = error "Empty list"
max' (x:[]) = x
max' (x:xs) = max x (max' xs)

--2 count positive numbers
countPos :: (Ord a, Num a) => [a] -> Int
countPos [] = error "List is empty"
countPos [x] = if x > 0 then 1 else 0
countPos (x:xs)
    | x > 0 = 1 + countPos xs
    | otherwise = 0 + countPos xs

--3 InRange - Returns a List that is limited to a upper and lower bound
inRange ::(Ord a ) => [a] -> a -> a -> [a]
inRange [] _ _ = []
inRange (x:[]) y z
    | x > y && x < z = [x]
inRange (x:xs) y z
    | x > y && x < z = x : inRange xs y z
    | otherwise = inRange xs y z

--4 length of a list
length' :: [a] -> Int
length' [] = 0
length' (x:[]) = 1
length' (x:xs) = 1 + length' xs

--5 replicate
replicate' :: Int -> Int -> [Int] 
replicate' 0 y = []
replicate' 1 y = [y]
replicate' x y = [y] ++ replicate' (x-1) y

--6 take
take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 [list] = [list]
take' 1 (x:xs) = [x]
take' y (x:xs) = x: take' (y-1) xs

--7 Reverse
reverse' :: [a] -> [a]
reverse'[] = []
reverse' (x:[]) = [x]
reverse' (x:xs) = (reverse' xs) ++ [x]

--8 Zip to get a list of tuples
zip' :: [a] -> [b] -> [(a,b)]
zip' [] (x:xs) = []
zip' (x:xs) []  = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

--9 Factorial
fac :: Int -> Int
fac 0 = 1
fac 1 = 1
fac x = x * fac (x - 1)

--10 quicksort
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) =  qsort [a | a <- xs, a <=x ] ++ [x] ++ qsort [b | b <-xs, b > x]

--11 Map
map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

--12 filter
filter' :: (a->Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs

--13 Capitilize all letter
capit :: String -> String -- [Char] -> [Char]
capit [] = []
capit [x] = [toUpper x]
capit (x:xs) = toUpper x : capit xs

--14 insert
insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x [y] = if x < y then x : [y] else y :[x]
insert x (y:ys)
       | x < y = x : y : ys
       | otherwise = y : insert x ys

--14.1
isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert x (isort xs)

--15 mod
mod' :: (Integral a, Ord a) => a -> a -> a
mod' x 1 = 0
mod' 1 x = 1
mod' x y
      | y > x = x 
      | Otherwise = last [x -(3 * a) | a <- [1..x] ]

--16 factors
sf :: (Integral a, Eq a) => a -> [a]
sf 0 = []
sf 1 = []
sf 2 = []
sf a = tail $ init (checkMod a)


-- Lambda expression
add :: Int -> Int -> Int -- add :: (Int -> (Int -> Int))
add = \x -> \y -> x + y 

xor :: Bool -> Bool -> Bool
xor = (/=) 


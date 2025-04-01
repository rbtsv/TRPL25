-- Шиманогов Игорь Николаевич
-- Integer, Int

sum' :: Integer -> Integer -> Integer
sum' a b = a + b

-- Integer -> (Integer -> Integer)
-- Bool

xor :: Bool -> Bool -> Bool
xor False True = True
xor True False = True
xor _ _ = False

-- maj3 :: Bool -> Bool -> Bool -> Bool
maj3 :: Bool -> Bool -> Bool -> Bool
maj3 True True _ = True
maj3 True _ True = True
maj3 _ True True = True
maj3 _ _ _ = False

min' :: Integer -> Integer -> Integer
min' x y
    | x < y = x
    | otherwise = y

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

step :: (Integer, Integer) -> Integer -> (Integer, Integer)
step x 0 = x
step (a, b) n = step (b, b+a) (n-1)

fib' :: Integer -> Integer
fib' n = snd $ step (0, 1) n

fib'' :: Integer -> Integer
fib'' n = snd $ step' (0, 1) n where
    step' x 0 = x
    step' (a, b) n = step' (b, a+b) (n-1)

-- Integer, Tuple, Bool, ->

type Quantity = Integer
newtype Quantity' = Quant Integer
data Color = Red | Blue | Green
data Cat = Cat Integer Integer Bool --age, weight, isAlive

getAge :: Cat -> Integer
getAge (Cat i _ _) = i

data Cat' = Cat' {
    age :: Integer,
    weight :: Integer,
    isAlive :: Bool
}

data IntList = Empty | Cons Integer IntList

tail' :: IntList -> IntList
tail' Empty = Empty
tail' (Cons x xs) = xs

len :: IntList -> Integer
len Empty = 0
len (Cons x xs) = 1 + len xs

concat' :: IntList -> IntList -> IntList
concat' Empty r = r
concat' (Cons x xs) r = Cons x (concat' xs r)

-- a -- тип, [a] -- список, в котором все элементы типа a
-- [], (h : t)
-- Заметка a + b, f a b, (+), `f`

ones :: [Integer]
ones = 1 : ones

primes :: [Integer]
primes = filterPrime [2,3..]

filterPrime :: [Integer] -> [Integer]
filterPrime (p : xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]

data IntTree = Leaf Integer | Parent Integer IntTree IntTree

-- data Tree a = Leaf' a | Parent' a (Tree a) (Tree a)

sumTree :: IntTree -> Integer
sumTree (Leaf x) = x
sumTree (Parent x l r) = x + sumTree l + sumTree r
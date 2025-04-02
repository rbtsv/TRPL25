data Expression =
    Var |
    Const Integer |
    Sum Expression Expression |
    Mult Expression Expression |
    Sin Expression |
    Cos Expression |
    Pow Expression Int
-- String = [Char]
showExp :: Expression -> String
showExp Var = "x"
showExp (Const i) = show i -- про Show мы поговорим отдельно
showExp (Sum a b) = "(" ++ showExp a ++ "+" ++ showExp b ++ ")"
showExp (Mult a b) = "(" ++ showExp a ++ "*" ++ showExp b ++ ")"
showExp (Sin a) = "sin(" ++ showExp a ++ ")"
showExp (Cos a) = "cos(" ++ showExp a ++ ")"
showExp (Pow a i) = "(" ++ showExp a ++ "^" ++ show i ++ ")"

exex :: Expression
exex = Mult (Sin (Pow Var 2)) (Sum (Cos Var) (Mult (Const 5) Var))

deriv :: Expression -> Expression
deriv Var = Const 1
deriv (Const _) = Const 0
deriv (Sum a b) = Sum (deriv a) (deriv b)
deriv (Mult a b) = Sum (Mult a $ deriv b) (Mult (deriv a) b)
deriv (Sin a) = Mult (Cos a) (deriv a)
deriv (Cos a) = Mult (Sin a) (deriv a)
deriv (Pow a i) = Mult (Pow a (i-1)) (deriv a)

-- fst :: (a, b) -> a
-- Параметрический полиморфизм
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- ($) :: (a -> b) -> a -> b

-- Полиморфные типы
-- a => [a]
-- kind
-- Int: kind *
-- []: kind * -> *

data BinTree a = Leaf a | Parent a (BinTree a) (BinTree a)

sumIntTree :: BinTree Integer -> Integer
sumIntTree (Leaf x) = x
sumIntTree (Parent x l r) = sumIntTree l + x + sumIntTree r

-- ad-hoc полиморфизм!

sumTree :: Num a => BinTree a -> a
sumTree (Leaf x) = x
sumTree (Parent x l r) = sumTree l + x + sumTree r

x = sumTree (Leaf 5)
x' = sumTree (Leaf 6.6)

merge :: Ord a => [a] -> [a] -> [a]
merge [] r = r
merge l [] = l
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort a) (mergeSort b) where
    (a, b) = halve xs

mergePairs :: Ord a => [[a]] -> [[a]]
mergePairs (s1 : s2 : ss) = merge s1 s2 : mergePairs ss
mergePairs ss = ss

mergeAll :: Ord a => [[a]] -> [a]
mergeAll [x] = x
mergeAll sorteds = mergeAll (mergePairs sorteds)

mergeSortBottomUp :: Ord a => [a] -> [a]
mergeSortBottomUp xs = mergeAll $ map (: []) xs

--Monoid
class Monoid m => Group m where
    inverse :: m -> m
-- пожалуйста, делайте Group только если это реально группа

instance Semigroup Bool where
    (<>) :: Bool -> Bool -> Bool
    False <> False = False
    True <> True = False
    _ <> _ = True
instance Monoid Bool where
    mempty :: Bool
    mempty = False
instance Group Bool where
    inverse :: Bool -> Bool
    inverse False = True
    inverse True = False

instance (Eq a) => Eq (BinTree a) where
    (==) :: Eq a => BinTree a -> BinTree a -> Bool
    (Leaf a) == (Leaf b) = a == b
    (Parent x lx rx) == (Parent y ly ry) = x == y && ly == lx && ry == rx
    _ == _ = False

data BinTree' a = Leaf' a | Parent' a (BinTree' a) (BinTree' a)
    deriving (Eq, Ord, Show)

-- f :: a -> b, x :: [a], map f a
-- Функтор!
-- fmap :: (a -> b) -> f a -> f b
-- map ::  (a -> b) -> [a] -> [b]
-- fmap id x = x, fmap (f . g) = fmap f . fmap g

instance Functor BinTree where
    fmap :: (a -> b) -> BinTree a -> BinTree b
    fmap f (Leaf x) = Leaf $ f x
    fmap f (Parent x l r) = Parent (f x) (fmap f l) (fmap f r)


treeex = Leaf 3.14
sq = fmap (\x -> x*x) treeex

instance Foldable BinTree where
    foldr :: (a -> b -> b) -> b -> BinTree a -> b
    foldr f s (Leaf x) = f x s
    foldr f s (Parent x l r) = foldr f (f x $ foldr f s r) l

example :: BinTree Int
example = Parent 1 (Parent 3 (Leaf 5) (Leaf 4)) (Parent 2 (Leaf 6) (Leaf 7))

sum' :: (Foldable f, Num b) => f b -> b
sum' = foldr (+) 0

-- Аппликативный функтор
-- Функтор, который нормально навешивается на стрелочные типы
-- pure :: a -> f a (kind: * -> *)
-- (<*>) :: f (a -> b) -> f a -> f b

-- (pure id) <*> v = v
-- (pure f) <*> (pure x) = (pure f x)
-- u <*> pure y = pure ($ y) <*> u
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)


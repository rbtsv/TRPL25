-- Мы уже сталкивались с полиморфными функциями.
-- Например curry:
-- curry :: ((a, b) -> c) -> a -> b -> c
-- Это означает, что для люьых типов a, b и c есть подобная функция.Applicative
-- Без ограничений на данный типы. Это называется параметрическим полиморфизмом.
-- Параметрически полиморфные функции обычно слишком "общие", но все еще очень полезные.
-- Например (.), ($), fst. 

-- Более того, у нас есть и полиморфные типы!
-- Рассмотрим понятие kind: вот у нас есть Int, это тип, уже готовый, его kind - *.
-- А есть вот списки, какой-бы не был тип a, мы можем сделать тип [a]
-- kind у cписка: * -> *. Он берет на вход тип и выдает новый тип.
-- Более того, "->" в типах функций, это тоже конструктор типа!
-- Он принимает тип a, тип b и выдает тип a->b. Его kind: * -> * -> *.

-- Давайте возьмём наш пример с бинарным деревом из прошлого семинара,
-- Но теперь разрешим в листья класть значения произвольного типа.
data BinTree a = Leaf a | Parent a (BinTree a) (BinTree a)
-- В чем теперь проблема? Например мы хотим написать функцию, которая считает сумму по дереву.
sumIntTree :: BinTree Integer -> Integer
sumIntTree (Leaf i) = i
sumIntTree (Parent c a b) = c + sumIntTree a + sumIntTree b
-- Эта функция определена только для деревьев с типом Integer?
-- А что если мы хотим складывать Float? А строки?
-- конечно можно sumIntTree :: BinTree a -> (a -> a -> a) -> a
-- Но обычно у типа подразумевается конкретное сложение!
-- Нас спасет ad-hoc полиморфизм!
-- Это ситуация, когда функция полиморфна с некоторыми ограничениями на типы.
-- В haskell для этого есть механизм type classes.
-- Он похож на интерфейсы в go или rust.
-- Что нужно, что бы эта функция была определна для типа BinTree a?
-- Что бы мы могли складывать элементы типа a!
-- Давайте узнаем, что требует функция (+)
-- (+) :: Num a => a -> a -> a
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/GHC-Num.html
-- Тогда можем написать так:
sumTree :: Num a => BinTree a -> a
sumTree (Leaf i) = i
sumTree (Parent c a b) = c + sumTree a + sumTree b
-- На самом деле, нам хватило бы и полугруппы
-- https://wiki.haskell.org/Monoid
-- Заметьте, что если библиотечные классы типов просят вас соблюдать некоторые "законы"
sumTreeM :: Semigroup a => BinTree a -> a
sumTreeM (Leaf i) = i
sumTreeM (Parent c a b) = sumTreeM a <> c <> sumTreeM b
-- Давайте напишем сортировку. В функциональных языках приятно писать merge.
-- Что будем сортировать? То, что умеем сравнивать
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Ord.html

merge :: Ord a => [a] -> [a] -> [a]
merge [] r = r
merge l [] = l
merge l@(lh:lt) r@(rh:rt)
    | lh < rh = lh : merge lt r
    | otherwise = rh : merge l rt

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort a) (mergeSort b) where
    (a, b) = halve xs

-- Можно и в обратную сторону: наша рекурсия же обрывается на одноэлементных списках
-- Давайте сразу их и будем объединять

mergePairs :: Ord a => [[a]] -> [[a]]
mergePairs (sorted1 : sorted2 : sorteds) = merge sorted1 sorted2 : mergePairs sorteds
mergePairs sorteds = sorteds

mergeAll :: Ord a => [[a]] -> [a]
mergeAll [sorted] = sorted
mergeAll sorteds = mergeAll (mergePairs sorteds)

mergeSortBottomUp :: Ord a => [a] -> [a]
-- (map (\x -> [x]) list)
mergeSortBottomUp list = mergeAll (map (: []) list)

-- Давайте теперь попробуем написать свой класс типов
class Monoid m => Group m where
    inverse :: m -> m
-- И населить его
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
    inverse True = False
    inverse False = True
-- Можно и рекурсивно
instance (Eq a) => Eq (BinTree a) where 
  (==) :: Eq a => BinTree a -> BinTree a -> Bool
  Leaf a            == Leaf b            =  a == b
  (Parent v1 l1 r1) == (Parent v2 l2 r2) =  (l1==l2) && (r1==r2) && (v1==v2)
  _                 == _                 =  False
-- Дефолтные реализации instance компилятор может иногда сделать сам
-- Для этого есть ключевое слово deriving
data BinTree' a = Leaf' a | Parent' a (BinTree' a) (BinTree' a) deriving (Eq, Ord, Show)
-- В ghci узнайте :instances BinTree', :instances BinTree' Bool, :instances BinTree' (BinTree Bool)

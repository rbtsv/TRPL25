-- Мы уже умеем писать функции.
-- Еще мы видели некоторые базовые типы: Integer, Tuple, Bool, стрелочные типы у функций.
-- Но что, если нам этого не хватает?
-- В общем случае есть ключевое слово data, после которого надо написать конструкторы типа.
data IntList = Empty | Cons Integer IntList
-- Для синонимов типов есть type и newtype.
type Qunatity = Integer
newtype TotalyNotAInteger = TNAInt Integer
-- Первый компилятор не отличает от Integer, второй отличает. У него свой конструктор, у него свои функции.
-- Самый простой вариант -- когда у конструкторов вообще нет аргументов, это тип-перечесление.
data FuzzyBool = FTrue | FFalse | NotSure
-- Еще есть named fields, похожие на структуры. Пусть есть тип с одним конструктором.
data Cat = Cat 
    Integer -- age
    Integer -- num_of_children
    Integer -- lives remains
--Как теперь узнать его возраст? Можно написать функцию. 
catAge :: Cat -> Integer
-- pattern matching может разбирать тип по конструктору!
catAge (Cat a _ _) = a
--Что бы не делать этого руками, можно сделать так:
data Cat' = Cat'
    { age :: Integer
    , children :: Integer
    , lives :: Integer
    }
-- Теперь у нас автоматически есть функция age :: Cat' -> Integer

-- Конечно, наиболее интересны для нас индуктивные типы:
-- такие, у которых один из конструкторов принимает на вход переменную этого типа.
-- Мы уже определили IntList. Давайте напишем для него пару простых функций.
tail' :: IntList -> IntList
tail' Empty = Empty
tail' (Cons _ t) = t 

len' :: IntList -> Integer
len' Empty = 0
len' (Cons _ t) = 1 + len' t

concat' :: IntList -> IntList -> IntList
concat' Empty x = x
concat' (Cons h t) x = Cons h $ concat' t x
-- Конечно, у нас уже есть встроенный тип для списков, это List!
-- Для него уже все определено. Это полиморфный тип, о них мы поговорим позднее.
-- Если у вас есть тип a, то [a] --- это список таких значений.
-- Для удобства конструкторы List называются [] и (h : t).
-- Помимо этого есть куча синтаксического сахара для pattern matching по спискам (f [a, b, c] = a).
-- Так как haskell ленивый, то никто не мешает быть спискам бесконечными. 

nat :: [Integer]
nat = 0 : map (1 +) nat
-- map :: (a -> b) -> [a] -> [b]

-- Я уже говорил, что мы любим синтаксический сахар?
nat' :: [Integer]
nat' = [0,1..]
-- Особенно удобный list comprehension
bigsquares :: [Integer]
bigsquares = [n*n | n <- nat, n > 200]
-- Решим пару задачек. Например, сделаем список простых чисел.
primes :: [Integer]
primes = filterPrime [2, 3..]
    where filterPrime (p : xs) = p : filterPrime [x | x <- xs, mod x p /= 0]
-- Из функции с 2 аргументами можно сделать оператор взяв ее в смешные кавычки ``
-- mod x p -> x `mod` p

-- Теперь выведем список всех пифагоровых троек
triplets :: [(Integer, Integer, Integer)]
triplets = [(x, y, z) | z <- [1..], y <- [1..z], x <- [1..y] , x*x + y*y == z*z]

--У типа может быть несколько конструкторов, принимающих на вход тот же тип.
--Так строится наиболее важный для нас пример -- деревья!
--Cамое простое -- бинарное дерево, у которго, например, в каждой вершине натуральное число.
data BinIntTree = Leaf Integer | Parent Integer BinIntTree BinIntTree

example :: BinIntTree
example = Parent 1 (Parent 3 (Leaf 5) (Leaf 4)) (Parent 2 (Leaf 6) (Leaf 7)) 
--Как посчитать сумму по дереву? Рекурсией!

sumTree :: BinIntTree -> Integer
sumTree (Leaf i) = i
sumTree (Parent c a b) = c + sumTree a + sumTree b

-- На семинарах вам уже рассказывали про символьное дифференцирование.
-- Пусть нам уже подали на вход дерево разбора выражения.
-- Задача: Найти его производную

data Expression = 
    Var |
    Const Integer |
    Sum Expression Expression |
    Mult Expression Expression | 
    Sin Expression |
    Cos Expression |
    Pow Expression Integer

-- Давайте научимся хотя бы выводить данные выражения в строку!
-- В Haskell String = [Char], так что работаем мы с ними как со списками
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

derivative :: Expression -> Expression
derivative Var = Const 1
derivative (Const _) = Const 0
derivative (Sum a b) = Sum (derivative a) (derivative b)
derivative (Sin a) = Mult (Cos a) (derivative a)
derivative (Cos a) = Mult (Sin a) (derivative a)
derivative (Pow a i) = Mult (Pow a (i-1)) (derivative a)
derivative (Mult a b) = Sum (Mult a $ derivative b) (Mult b $ derivative a)

simplify :: Expression -> Expression
simplify (Mult _ (Const 0)) = Const 0
simplify (Mult (Const 0) _) = Const 0
simplify (Mult (Const 1) a) = simplify a
simplify (Mult a (Const 1)) = simplify a
simplify (Sum a (Const 0)) = simplify a 
simplify (Sum (Const 0) a) =  simplify a
simplify (Pow a 1) = simplify a
simplify Var = Var
simplify (Const t) = Const t
simplify (Sum a b) = Sum (simplify a) (simplify b)
simplify (Sin a) = Sin (simplify a)
simplify (Cos a) = Cos (simplify a)
simplify (Pow a i) = Pow (simplify a) i
simplify (Mult a b) = Mult (simplify a) (simplify b)

-- finalize :: Expression -> Expression
-- finalize x 
--    | (simplify x) == x = x
--    | otherwise = simplify x
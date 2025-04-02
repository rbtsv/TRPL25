import Control.Monad 
-- Сейчас мы поговорим о стандартных для haskell классах типов
-- Самое простое, что есть - функтор.
-- Мы все знаем что такое map :: (a -> b) -> [a] -> [b]
-- Функтор - это обобщение списка, в том смысле, что функтор это то, по чему можно "маппиться"
-- Для этого нужно реализовать fmap :: (a -> b) -> f a -> f b
-- От него ожидается следующее fmap id = id и fmap (f . g)  ==  fmap f . fmap g
data BinTree a = Leaf a | Parent a (BinTree a) (BinTree a) deriving (Show, Eq)
instance Functor BinTree where
    fmap :: (a -> b) -> BinTree a -> BinTree b
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Parent a l r) = Parent (f a) (fmap f l) (fmap f r)

square :: (Functor f, Num b) => f b -> f b
square = fmap (\x -> x * x)

-- Еще есть foldable, он обобщает foldr :: (a -> b -> b) -> b -> t a -> b
-- Например, length :: Foldable t => t a -> Int

instance Foldable BinTree where
    foldr :: (a -> b -> b) -> b -> BinTree a -> b
    foldr f s (Leaf x) = f x s
    foldr f s (Parent x l r) = foldr f (f x $ foldr f s l) r

example :: BinTree Int
example = Parent 1 (Parent 3 (Leaf 5) (Leaf 4)) (Parent 2 (Leaf 6) (Leaf 7))

sum' :: (Foldable f, Num b) => f b -> b
sum' = foldr (+) 0

-- Аппликативный функтор - это функтор, который "можно навесть на стрелочные типы"
-- class (Functor f) => Applicative f where
--    pure  :: a -> f a
--    (<*>) :: f (a -> b) -> f a -> f b
-- Ожидаемое поведение
-- pure id <*> v = v                            -- Identity
-- pure f <*> pure x = pure (f x)               -- Homomorphism
-- u <*> pure y = pure ($ y) <*> u              -- Interchange
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition

top :: BinTree a -> a
top (Leaf x) = x
top (Parent x _ _) = x

instance Applicative BinTree where
    pure :: a -> BinTree a
    pure x = Parent x (pure x) (pure x)

    (<*>) :: BinTree (a -> b) -> BinTree a -> BinTree b
    (Leaf f) <*> v = Leaf $ f $ top v
    f <*> (Leaf v) = Leaf $ top f v
    (Parent f lf rf) <*> (Parent v lv rv) = Parent (f v) (lf <*> lv) (rf <*> rv)

-- pure f <*> v = fmap f v = f <$> v

-- Вообще у Foldable и Functor есть ребёнок под названием Traversable, но опустим его
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- Но не будем про него, двинемся к более важному примеру.
-- Монады.
-- Монада наследуется от аппликативного функтора (уже более 10 лет)
-- Дополнительно нужно определить (>>=) :: m a -> (a -> m b) -> m b
-- Аналогия со списками: если вы знаете, как элемента сделать список, то из списка элементов можно собрать новый список.
-- К сожалению, я не вижу хорошего способа определить на таком дереве структуру монады
-- По этому давайте переходить к классическим примерам!

data Maybe' a = Just' a | Nothing'
instance Functor Maybe' where
    fmap :: (a -> b) -> Maybe' a -> Maybe' b
    fmap f Nothing' = Nothing'
    fmap f (Just' a) = Just' $ f a
instance Applicative Maybe' where
    pure :: a -> Maybe' a
    pure = Just'
    (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
    f <*> Nothing' = Nothing'
    Nothing' <*> a = Nothing'
    Just' f <*> Just' a = Just' $ f a
instance Monad Maybe' where
    (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
    Nothing' >>= f = Nothing'
    Just' x >>= f = f x

-- пример 
divM :: Float -> Float -> Maybe Float
divM _ 0 = Nothing
divM a b = Just $ a / b
-- пример из https://learnyouahaskell.com/a-fistful-of-monads
type Birds = Integer
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

-- return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2

routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second

-- https://en.wikibooks.org/wiki/Haskell/do_notation

listOfTuples :: [(Integer, Char)]
listOfTuples = do
    x <- [1..10]
    y <- ['A'..'Z']
    return (x, y) -- return = pure

-- Maybe, List, кто еще? State и IO! Но об этом (возможно) позже
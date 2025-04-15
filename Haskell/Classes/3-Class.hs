import GHC.Base (liftM2)
-- Монада

-- map :: (a -> b) -> f a -> f b
-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b
-- bind (>>=) :: f a -> (a -> f b) -> f b

-- Список - монада!

-- div :: Int -> Int -> Int
data Maybe' a = Just' a | Nothing'
instance Functor Maybe' where
    fmap :: (a -> b) -> Maybe' a -> Maybe' b
    fmap f Nothing' = Nothing'
    fmap f (Just' x) = Just' $ f x
instance Applicative Maybe' where
    pure :: a -> Maybe' a
    pure = Just'
    (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
    f <*> Nothing' = Nothing'
    Nothing' <*> x = Nothing'
    Just' f <*> Just' x = Just' $ f x
instance Monad Maybe' where
    (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
    Nothing' >>= f = Nothing'
    Just' x >>= f = f x

divM :: Float -> Float -> Maybe Float
divM _ 0 = Nothing
divM a b = Just $ a / b

type Birds = Integer
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - (right+n)) < 4 = Just (left, right + n)
    | otherwise = Nothing

routine :: Maybe Pole
routine = do
    start <- pure (0, 0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 2 second

-- pure (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2

-- m a -> a
-- a -> m a

listOfTuples :: [(Integer, Char)]
listOfTuples = do
    x <- [1..10]
    y <- ['A'..'Z']
    pure (x, y)

-- List, Maybe..? State!
newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap fn (State sa) = State (\s0 -> let (a, s1) = sa s0 in (fn a, s1))
instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State $ \s -> (a, s)
    (<*>) :: State s (a -> b) -> State s a -> State s b
    (<*>) = undefined
instance Monad (State s) where
    (>>=) :: State s a -> (a -> State s b) -> State s b
    State act >>= k = State $ \s -> let (a, s') = act s in runState (k a) s'

type Stack = [Int]
empty :: Stack
empty = []

pop :: State Stack Int
-- newtype State s a = State {runState :: s -> (a, s)}
pop = State $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((), a:xs)

tos :: State Stack Int
tos = State $ \(x:xs) -> (x, x:xs)

stackManip :: State Stack Int
stackManip = do
    push 10
    push 20
    a <- pop
    b <- pop
    push (a + b)
    tos


humanManip :: IO ()
humanManip = do
    a <- getLine
    b <- getLine
    let x = read a
    let y = read b
    print (x + y)

helloWorld :: IO ()
helloWorld = print "Hello World!"

-- main :: IO () 

-- type Parser = String -> Tree
-- type Parser = String -> (Tree, String)
-- type Parser = String -> [(Tree, String)]
-- type Parser a = String -> [(a, String)]
-- type Parser s a = s -> [(a, s)] :(
newtype Parser a = Parser {runParse :: String -> Maybe (a, String)}

pureP :: a -> Parser a
pureP v = Parser $ \inp -> Just (v, inp)
zero :: Parser a
zero = Parser $ const Nothing
item :: Parser Char
item = Parser $ \inp  -> case inp of
    [] -> Nothing
    (x:xs) -> Just (x, xs)
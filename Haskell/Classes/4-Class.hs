{-# LANGUAGE MonadComprehensions #-}
import Control.Monad (ap, liftM, MonadPlus)
import Control.Applicative (Alternative (..))
import Data.Char (digitToInt)
import GHC.Base (ord)
-- type Parser = String -> Tree
-- type Parser = String -> (Tree, String)
-- type Parser = String -> [(Tree, String)]
-- type Parser a = String -> [(a, String)]
-- type Parser s a = s -> [(a, s)]
-- type Parser a = String -> Maybe (a, String)
newtype Parser a = Parser {runParse :: String -> Maybe (a, String)}

pureP :: a -> Parser a
pureP v = Parser $ \inp -> Just (v, inp)
zero :: Parser a
zero = Parser $ const Nothing
item :: Parser Char
item = Parser $ \inp -> case inp of
    [] -> Nothing
    (x:xs) -> Just (x, xs)

bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = Parser $ \inp -> case runParse p inp of
    Nothing -> Nothing
    Just (v, inp') -> runParse (f v) inp'

-- >>= !

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap = liftM

instance Applicative Parser where
    pure :: a -> Parser a
    pure = pureP
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) = ap

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) = bind

instance Alternative Parser where
    empty :: Parser a
    empty = zero
    (<|>) :: Parser a -> Parser a -> Parser a
    Parser l <|> Parser r = Parser $ \inp ->
        case l inp of
            Nothing -> 
                case r inp of
                    Nothing -> Nothing
                    rs -> rs
            ls -> ls
        
instance MonadPlus Parser

sat :: (Char -> Bool) -> Parser Char
sat f = item >>= (\x -> if f x then pure x else zero)

char :: Char -> Parser Char
char c = sat (==c)

lower :: Parser Char
lower = sat (`elem` ['a'..'z'])

upper :: Parser Char
upper = sat (`elem` ['A'..'Z'])

alpha :: Parser Char
alpha = lower <|> upper

num :: Parser Char
num = sat (`elem` ['0'..'9'])

alphanum :: Parser Char 
alphanum = alpha <|> num

satstring' :: [Char -> Bool] -> Parser String
satstring' [] = pure ""
satstring' (x:xs) = sat x >>= (\y -> satstring' xs >>= \ys -> pure (y:ys))

satstring :: [Char -> Bool] -> Parser String
satstring [] = pure ""
satstring (x : xs)= do 
    y <- sat x
    ys <- satstring xs
    return (y:ys)

word :: String -> Parser String
word s = satstring $ map (==) s

star1 :: Parser a -> Parser [a]
star1 p = do
    y <- p
    ys <- star1 p <|> pure []
    return $ y : ys

star :: Parser a -> Parser [a]
star p = star1 p <|> pure []

try :: Parser a -> Parser ()
try p = Parser $ \inp ->
    case runParse p inp of
        Nothing -> Nothing
        _ -> Just ((), inp)

data Expression =
    Var |
    Const Int |
    Sum Expression Expression |
    Mult Expression Expression |
    Sin Expression |
    Cos Expression |
    Pow Expression Int
    deriving (Eq, Show)

varParser :: Parser Expression
varParser = do
    char 'x'
    return Var

dgToInt :: Char -> Int
dgToInt x = ord x - ord '0' 

natParser :: Parser Int
natParser = do
    n <- star1 num
    let ns = map dgToInt n
    return $ foldl (\x y -> x * 10 + y) 0 ns

intParser :: Parser Int
intParser = natParser <|> (char '-' *> fmap (*(-1)) natParser)

constParser :: Parser Expression
constParser = fmap Const intParser

sumParser :: Parser Expression
sumParser = do
    char '('
    x <- exprParser
    char '+'
    y <- exprParser
    char ')'
    return $ Sum x y

mulParser :: Parser Expression
mulParser = do
    char '('
    x <- exprParser
    char '*'
    y <- exprParser
    char ')'
    return $ Mult x y

sinParser :: Parser Expression
sinParser = fmap Sin $ word "sin(" *> exprParser <* char ')'

cosParser :: Parser Expression
cosParser = fmap Cos $ word "cos(" *> exprParser <* char ')'

powParser :: Parser Expression
powParser = do 
    char '('
    x <- exprParser
    char '^'
    i <- intParser
    char ')'
    return $ Pow x i

exprParser :: Parser Expression
exprParser = varParser <|> constParser <|> sumParser <|> mulParser <|>
    sinParser <|> cosParser <|> powParser

ex :: String
ex = "sin(((3+(x^5))*6))"
-- Just (Sin (Mult (Sum (Const 3) (Pow Var 5)) (Const 6)),"")

listComp :: [Integer]
listComp = [x+y | x <- [1..10], y <- [10..15]]

listComp' :: [Integer]
listComp' = do
    x <- [1..10]
    y <- [10..15]
    return $ x + y

mulParser' :: Parser Expression
mulParser' = do
    char '('
    x <- exprParser
    char '*'
    y <- exprParser
    char ')'
    return $ Mult x y

mulParser'' :: Parser Expression
mulParser'' = [Mult x y | 
    x <- char '(' *> exprParser <* char '*',
    y <- exprParser <* char ')']
import Control.Monad (ap, liftM)
import Control.Applicative (Alternative)
import GHC.Base (Alternative(..))
-- type Parser = String -> Tree
-- type Parser = String -> (Tree,String)
-- type Parser = String -> [(Tree,String)]
-- type Parser a = String -> [(a,String)]
-- type Parser s a = s -> [(a, s)]

newtype Parser a = Parser {runParse :: String -> [(a,String)]}

pureP :: a -> Parser a
--pureP v = \inp -> [(v, inp)]
pureP v =  Parser $ \inp -> [(v, inp)]
zero :: Parser a
zero  = Parser $ const []
item :: Parser Char
item = Parser $ \inp -> case inp of 
    [] -> []
    (x:xs) -> [(x,xs)]

seq :: Parser a -> Parser b -> Parser (a,b)
p `seq` q = Parser $ \inp -> [((v,w),inp'') | (v,inp') <- runParse p inp,
                                              (w,inp'') <- runParse q inp']

bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = Parser $ \inp -> concat [runParse (f v) inp | (v,inp') <- runParse p inp]

-- Отсюда уже видно, что Parser -- это монада!
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
    Parser l <|> Parser r = Parser $ \input ->
        case l input of
        [] ->
            case r input of
            [] -> []
            rs -> rs
        ls -> ls

sat :: (Char -> Bool) -> Parser Char
sat f = item >>= \x ->
    if f x then pure x else zero
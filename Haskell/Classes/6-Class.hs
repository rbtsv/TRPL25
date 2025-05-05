import Control.Monad.Trans.State (StateT (..))
import Control.Monad (ap, MonadPlus(..))
import Control.Applicative (Alternative(..))
import Control.Monad.Trans (MonadTrans(lift))
import Data.IntMap (update)
import Data.Char (ord)
import System.IO (IOMode(ReadMode),openFile)
import GHC.IO.Handle (hGetContents)

type Parser a = StateT String Maybe a

item :: Parser Char
item = StateT $ \inp -> case inp of
    [] -> Nothing
    (x:xs) -> Just (x,xs)

sat :: (Char -> Bool) -> Parser Char
sat f = item >>= (\x -> if f x then pure x else mzero)

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

(+*+) :: Parser [a] -> Parser [a] -> Parser [a]
a +*+ b = fmap (++) a <*> b

type Member = (String, Value)

data Value = Object [Member]
            | Array [Value]
            | Number Int
            | Str String 
            | Bl Bool
            | Null
    deriving Show

wsP :: Parser String
wsP = star $ char '\SP' <|> char '\HT' <|> char '\LF' <|> char '\CR'

valueP :: Parser Value
valueP = nullP <|> boolP <|> arrayP <|> stringP <|> objectP <|> numP

nullP :: Parser Value
nullP = do
    word "null"
    return Null

trueP :: Parser Value
trueP = do
    word "true"
    return $ Bl True

falseP :: Parser Value
falseP = do
    word "false"
    return $ Bl False

boolP :: Parser Value
boolP = trueP <|> falseP

jsonP :: Parser Value
jsonP = wsP *> valueP <* wsP

arrayP :: Parser Value
arrayP = do
    wsP *> char '[' <* wsP
    v <- valueP
    let vs = wsP *> char ',' <* wsP
    rest <- star (vs *> valueP)
    wsP
    char ']'
    wsP
    return $ Array (v : rest)

safeStringP :: Parser String
safeStringP = star $ sat (/='"')

stringP :: Parser Value
stringP = do
    char '"'
    val <- safeStringP
    char '"'
    return $ Str val
    
memberP :: Parser Member
memberP = do
    char '\"'
    s <- safeStringP
    char '\"'
    wsP
    char ':'
    wsP
    v <- valueP
    return (s, v)

objectP :: Parser Value
objectP = do
    wsP
    char '{'
    wsP
    m <- memberP 
    let vs = wsP *> char ',' <* wsP
    rest <- star (vs *> memberP)
    wsP
    char '}'
    wsP
    return $ Object (m : rest)

dgToInt :: Char -> Int
dgToInt x = ord x - ord '0' 

natParser :: Parser Int
natParser = do
    n <- star1 num
    let ns = map dgToInt n
    return $ foldl (\x y -> x * 10 + y) 0 ns

intParser :: Parser Int
intParser = natParser <|> (char '-' *> fmap (*(-1)) natParser)

numP :: Parser Value
numP = fmap Number intParser

example :: IO String
example = do
    file <- openFile "./example.json" ReadMode
    contents <- hGetContents file
    let value = runStateT jsonP contents 
    case value of
        Nothing -> return "sorry"
        Just x -> return $ show x 
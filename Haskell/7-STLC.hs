import Control.Monad.State.Lazy (StateT (..), get, put, MonadPlus (mzero), evalStateT)
import GHC.Base (Alternative(..))
import Data.Functor (($>))
-- Типы
data Ty = TyBool | TyArrow Ty Ty
    deriving Eq
instance Show Ty where
    show :: Ty -> String
    show TyBool = "Bool"
    show (TyArrow a b)= "("++ show a ++ " -> " ++ show b ++ ")"
-- Значения
data Value = TmTrue | TmFalse | TmAbs String Ty Tm -- редукция останавливаеся на абстракции
instance Show Value where
    show :: Value -> String
    show TmTrue = "True"
    show TmFalse = "False"
    show (TmAbs n t v) = "\\" ++ n ++ ":" ++ show t ++ " => " ++ show v
-- Термы
data Tm = TmValue Value
        | TmApp Tm Tm
        | TmVar String
        | TmIf Tm Tm Tm
instance Show Tm where
    show :: Tm -> String
    show (TmValue x) = show x
    show (TmVar x) = x
    show (TmIf x y z) = "if " ++ show x ++ " then " ++ show y ++ " else " ++ show z
    show (TmApp f a) = "(" ++ show f ++ ") (" ++ show a ++ ")"

-- Примеры:
idB :: Value
idB = TmAbs "x" TyBool $ TmVar "x"
idBB :: Value
idBB =  TmAbs "x" (TyArrow TyBool TyBool) $ TmVar "x"

-- подставляем s вместо x в последний аргумент
-- мы хотим подставлять в замкнутые термы, где нет свободных аргументов
substitute :: String -> Tm -> Tm -> Tm
substitute x s (TmVar y) = if x == y then s else TmVar y
substitute x s (TmApp f a) = TmApp (substitute x s f) (substitute x s a)
substitute x s (TmValue TmTrue) = TmValue TmTrue
substitute x s (TmValue TmFalse)= TmValue TmFalse
substitute x s (TmIf c t e) = TmIf (substitute x s c) (substitute x s t) (substitute x s e)
substitute x s (TmValue (TmAbs y p t))
    | x == y = TmValue $ TmAbs x p t
    | otherwise = TmValue $ TmAbs y p $ substitute x s t

step :: Tm -> Tm
step (TmIf (TmValue TmTrue) t e) = t
step (TmIf (TmValue TmFalse) t e) = e
step (TmIf c t e) = TmIf (step c) t e
step (TmApp (TmValue (TmAbs x t r)) (TmValue a)) = substitute x (TmValue a) r
step (TmApp (TmValue f) a) = TmApp (TmValue f) $ step a
step (TmApp f a) = TmApp (step f) a

bigstep :: Tm -> Value
bigstep (TmValue x) = x
bigstep x = bigstep $ step x

neg = TmAbs "x" TyBool $ TmIf (TmVar "x") (TmValue TmFalse) (TmValue TmTrue)

ex1 = TmApp (TmValue neg) (TmValue TmTrue)

-- теперь давайте учиться проверять типы

type Context = String -> Maybe Ty

defaultContext :: Context
defaultContext x = Nothing

updateContext :: Context -> String -> Ty -> Context
updateContext c n t x = if x == n then Just t else c x

inferTyFromContext :: Tm -> StateT Context Maybe Ty
inferTyFromContext (TmVar x) = do
    c <- get
    let tx = c x
    maybe mzero return tx
inferTyFromContext (TmValue TmTrue) = do return TyBool
inferTyFromContext (TmValue TmFalse) = do return TyBool
inferTyFromContext (TmIf c t e) = do
    ct <- inferTyFromContext c
    tt <- inferTyFromContext t
    et <- inferTyFromContext e
    if (tt == et) && (ct == TyBool) then return tt else mzero
inferTyFromContext (TmApp f a) = do
    ft <- inferTyFromContext f
    at <- inferTyFromContext a
    case ft of
        TyArrow at rt -> return rt
        _ -> mzero
inferTyFromContext (TmValue (TmAbs xn xt y)) = do
    c <- get
    put $ updateContext c xn xt
    yt <- inferTyFromContext y
    return $ TyArrow xt yt

inferTy :: Tm -> Maybe Ty
inferTy t = evalStateT (inferTyFromContext t) defaultContext

type Parser a = StateT String Maybe a

item :: Parser Char
item = StateT $ \inp -> case inp of
    [] -> Nothing
    (x:xs) -> Just (x,xs)

sat :: (Char -> Bool) -> Parser Char
sat f = item >>= (\x -> if f x then pure x else mzero)

lower :: Parser Char
lower = sat (`elem` ['a'..'z'])

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

arrowP :: Parser Ty
arrowP = do
    word "("
    x <- tyP
    word " -> "
    y <- tyP
    word ")"
    return $ TyArrow x y

tyP :: Parser Ty
tyP = word "Bool" $> TyBool <|> arrowP

absP :: Parser Value
absP = do
    word "\\"
    n <- star1 lower
    word ":"
    t <- tyP
    word " => "
    TmAbs n t <$> tmP

valueP :: Parser Value
valueP = word "True" $> TmTrue
    <|>  word "False" $> TmFalse
    <|>  absP

tmP :: Parser Tm
tmP = fmap TmValue valueP <|> undefined
import Control.Monad.State (StateT, MonadPlus (mzero), get, put, evalStateT)
data Ty = TyBool | TyArrow Ty Ty
    deriving Eq
instance Show Ty where
    show :: Ty -> String
    show TyBool = "Bool"
    show (TyArrow x y) = "(" ++ show x ++ " -> " ++ show y ++ ")"

data Value = TmTrue | TmFalse | TmAbs String Ty Tm
instance Show Value where
    show :: Value -> String
    show TmTrue = "True"
    show TmFalse = "False"
    show (TmAbs n t v) = "\\" ++ n ++ ":" ++ show t ++ " => " ++ show v

data Tm = TmValue Value
        | TmVar String
        | TmApp Tm Tm
        | TmIf  Tm Tm Tm
instance Show Tm where
    show :: Tm -> String
    show (TmValue x) = show x
    show (TmVar x) = x
    show (TmApp f x) = "(" ++ show f ++ ") (" ++ show x ++ ")"
    show (TmIf c t e) = "if " ++ show c ++ " then " ++ show t ++ " else " ++ show e
    
idB :: Value
idB = TmAbs "x" TyBool $ TmVar "x"
idBB :: Value
idBB = TmAbs "x" (TyArrow TyBool TyBool) $ TmVar "x"

substitute :: String -> Tm -> Tm -> Tm
substitute x s (TmVar y) = if x == y then s else TmVar y
substitute x s (TmApp f a) = TmApp (substitute x s f) (substitute x s a)
substitute x s (TmValue TmFalse) = TmValue TmFalse
substitute x s (TmValue TmTrue) = TmValue TmTrue
substitute x s (TmIf c t e) = TmIf (substitute x s c) (substitute x s t) (substitute x s e)
substitute x s (TmValue (TmAbs y t v))
    | x == y = TmValue $ TmAbs x t v
    | otherwise = TmValue $ TmAbs y t $ substitute x s v

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

neg :: Value
neg = TmAbs "x" TyBool $
        TmIf (TmVar "x")
            (TmValue TmFalse)
            (TmValue TmTrue)

ex1 :: Tm
ex1 = TmApp (TmValue neg) (TmValue TmTrue)

type Context = String -> Maybe Ty

defaultContext :: Context
defaultContext x = Nothing

updateContext :: Context -> String -> Ty -> Context
updateContext c n t x = if x == n then Just t else c x

inferTyFromContext :: Tm -> StateT Context Maybe Ty
inferTyFromContext (TmVar x) = do
    c <- get
    let tx = c x
    case tx of
        Just t -> return t
        Nothing -> mzero
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
        TyArrow xt rt -> if xt == at then return rt else mzero
        _ -> mzero
inferTyFromContext (TmValue (TmAbs xn xt y)) = do
    c <- get
    put $ updateContext c xn xt
    yt <- inferTyFromContext y
    return $ TyArrow xt yt

inferTy :: Tm -> Maybe Ty
inferTy x = evalStateT (inferTyFromContext x) defaultContext

-- TODO: написать парсер для этого языка
-- слабое интуиционисткая логика

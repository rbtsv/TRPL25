import Control.Monad (liftM, ap)
import Control.Monad.Trans ( MonadTrans(..) )
import Control.Monad.Trans.State ( StateT(..) )
import GHC.Base ( Alternative(..), MonadPlus (mzero) )
import Data.Char (ord)
-- Напишем функцию, которая принимает от пользователя строку
-- И возвращает Nothing, если она пустая, а иначе её

--readNN' :: Maybe (IO String)
--readNN' = do 
--    x <- getLine
--    case x of 
--        "" -> empty
--        x -> pure

--Ой... А что делать, если мы хотим иметь поведение двух монад сразу? Монадный трансформер!

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Functor (MaybeT m) where
    fmap :: Monad m => (a -> b) -> MaybeT m a -> MaybeT m b
    fmap = liftM
instance Monad m => Applicative (MaybeT m) where
    pure :: Monad m => a -> MaybeT m a
    pure = MaybeT . return . Just
    (<*>) :: Monad m => MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
    (<*>) = ap
instance Monad m => Monad (MaybeT m) where
    (>>=) :: Monad m => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    x >>= f = MaybeT $ do maybe_value <- runMaybeT x
                          case maybe_value of
                            Nothing    -> return Nothing
                            Just value -> runMaybeT $ f value
instance Monad m => Alternative (MaybeT m) where
    empty :: Monad m => MaybeT m a
    empty   = MaybeT $ return Nothing
    (<|>) :: Monad m => MaybeT m a -> MaybeT m a -> MaybeT m a
    x <|> y = MaybeT $ do maybe_value <- runMaybeT x
                          case maybe_value of
                               Nothing    -> runMaybeT y
                               Just _     -> return maybe_value
instance Monad m => MonadPlus (MaybeT m)

instance MonadTrans MaybeT where
    lift :: Monad m => m a -> MaybeT m a
    lift = MaybeT . fmap Just


readNN :: MaybeT IO String
readNN = do
    x <- lift getLine
    case x of
        "" -> empty
        x -> pure x

-- Есть куча монадных трансформеров в стандартной библиотеке!
-- Нас интересует прежде всего StateT s m a = StateT { runStateT :: (s -> m (a,s)) }
-- Заметьте, как это похоже на парсер!
newtype OldParser a = OldParser {runOldParse :: String -> Maybe (a, String)}
type Parser a = StateT String Maybe a
-- Заметьте, что все варианты можно теперь получить просто подменяя Maybe на [] или Either
-- Теперь haskell соберет структуру MonadPlus на нашем типе сам!
-- Единственное, на что мы опирались до этого, что придется определить -- item 
--item :: Parser Char
--item = Parser $ \inp -> case inp of
--    [] -> Nothing
--    (x:xs) -> Just (x,xs)

item :: Parser Char
item = StateT $ \s -> case s of
    (c:cs) -> Just (c, cs)
    []     -> Nothing

-- И этого хватит, что бы спарсить любой PEG!
-- Потому что 

eps :: Parser String
eps = pure ""
char :: Char -> Parser String
char c = item >>= \x -> if c==x then pure [x] else mzero
choice :: Parser String -> Parser String -> Parser String
choice a b = a <|> b
seq :: Parser String -> Parser String -> Parser String
seq a b = fmap (++) a <*> b
try :: Parser String -> Parser String
try a = StateT $ \s -> case runStateT a s of
    Nothing -> Nothing
    _ -> Just ("", s)


{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
import Control.Applicative (Alternative(..))
import Control.Monad (ap, MonadPlus(..))
import Control.Monad.Cont (liftM, MonadTrans(..))
import Control.Monad.Trans.State (StateT (..))
-- readNN :: Maybe (IO String)
-- readNN = do
--     x <- getLine
--     case x of
--         "" -> empty
--         x -> pure

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Monad m => Functor (MaybeT m) where
    fmap :: Monad m => (a -> b) -> MaybeT m a -> MaybeT m b
    fmap = liftM

instance Monad m => Applicative (MaybeT m) where
    pure :: Monad m => a -> MaybeT m a
    pure x = MaybeT $ return $ Just x
    -- pure = MaybeT. return . Just
    (<*>) :: Monad m => MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
    (<*>) = ap

instance Monad m => Monad (MaybeT m) where 
    (>>=) :: Monad m => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    x >>= f = MaybeT $ do
        maybe_value <- runMaybeT x
        case maybe_value of 
            Nothing -> return Nothing
            Just value -> runMaybeT $ f value

instance Monad m => Alternative (MaybeT m) where
    empty :: Monad m => MaybeT m a
    empty = MaybeT . return $ Nothing
    (<|>) :: Monad m => MaybeT m a -> MaybeT m a -> MaybeT m a
    x <|> y = MaybeT $ do 
        maybe_value_x <- runMaybeT x
        case maybe_value_x of
            Nothing -> runMaybeT y
            Just _ -> return maybe_value_x

instance Monad m => MonadPlus (MaybeT m) where

instance MonadTrans MaybeT where
    lift :: Monad m => m a -> MaybeT m a
    lift = MaybeT . fmap Just

readNN' :: MaybeT IO String
readNN' = do
    x <- lift getLine
    case x of 
        "" -> empty
        x -> return x

-- newtype StateT s m a = StateT {runStateT :: (s -> m (a, s))}
-- newtype Parser a = Parser {runParse :: String -> Maybe (a, String)}

type Parser a = StateT String Maybe a
item :: Parser Char
item = StateT $ \inp -> case inp of
    [] -> Nothing
    (x:xs) -> Just (x,xs)
eps :: Parser String
eps = pure ""
char :: Char -> Parser String
char c = item >>= (\x -> if c==x then pure [x] else empty)
choice :: Parser String -> Parser String -> Parser String
choice = (<|>)
(+/+) :: Parser String -> Parser String -> Parser String
a +/+ b = fmap (++) a <*> b
try :: Parser String -> Parser String
try a = StateT $ \s -> case runStateT a s of
    Nothing -> Just ("", s)
    _ -> Nothing


parserA :: Parser String
parserA = char 'a' +/+ parserA <|> char 'a'
parserB = char 'a' +/+ (char 'b' +/+ char 'b') <|> char 'b'
parserC = char 'c'
parserS = (parserA +/+ parserB) <|> (parserB +/+ parserC)


-- newtype StateT s m a = StateT {runStateT :: (s -> m (a, s))}
-- newtype ReaderT s m a = ReaderT {runReaderT :: (s -> m (a, s))}

-- parsec!
-- 
{-# LANGUAGE InstanceSigs #-}

module State where

import qualified Control.Monad                 as M

newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }
-- Připomínám: runStateT :: StateT s m a -> s -> m (s, a)

instance Monad m => Functor (StateT s m) where
    fmap :: (a -> b) -> StateT s m a -> StateT s m b
    fmap f stavovyVypocet = StateT $ \s -> do    -- Nový stavový výpočet bere stav
        (s', a) <- runStateT stavovyVypocet s    -- spustí starý výpočet, čímž dostane nový stav a ačko 
        return (s', f a)                         -- a vrátí nový stav a 'f a :: b'

instance Monad m => Monad (StateT s m) where
    return :: a -> StateT s m a
    return a = StateT $ \s -> return (s, a)

    (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    stavovyVypocet >>= f = StateT $ \s -> do
        (s', a) <- runStateT stavovyVypocet s  -- spustíme starý výpočet, dostaneme nový stav a ačko
        let stavovyVypocet' = f a              -- aplikací f na a dostaneme nový stavový výpočet
        runStateT stavovyVypocet' s'           -- který spustíme aplikací na nový stav

-- | Vrátí aktuální stav
get' :: Monad m => StateT s m s
get' = StateT $ \s -> return (s, s)

-- | Nastaví nový stav
set :: Monad m => s -> StateT s m ()
set s = StateT $ \z -> return (s, ())

-- | Změní stav pomocí dodané funkce
modify :: Monad m => (s -> s) -> StateT s m ()
modify f = StateT $ \s -> return (f s, ())

-- | Spustí stavový výpočet s počátečním stavem,
-- vrátí konečný stav.
evalState :: Monad m => s -> StateT s m a -> m s
evalState initialState action = do
    result <- runStateT action initialState
    return $ fst result

-- | Tuto instanci potřebujeme, protože Applicative je nadtřída Monad.
-- Reálně ji ale můžeme definovat pomocí Monad samotné, tedy tak učiňme :)
instance Monad m => Applicative (StateT s m) where
    pure  = return
    (<*>) = M.ap

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

instance Monad m => Functor (ExceptT e m) where
    fmap :: (a -> b) -> ExceptT e m a -> ExceptT e m b
    fmap f errorovyVypocet =
        ExceptT $ fmap (fmap f) (runExceptT errorovyVypocet)

instance Monad m => Monad (ExceptT e m) where
    return a = ExceptT $ return $ Right a
    m >>= k = ExceptT $ do
        a <- runExceptT m
        case a of
            Left  e -> return (Left e)
            Right x -> runExceptT (k x)

-- | Tuto instanci potřebujeme, protože Applicative je nadtřída Monad.
-- Reálně ji ale můžeme definovat pomocí Monad samotné, tedy tak učiňme :)
instance Monad m => Applicative (ExceptT s m) where
    pure  = return
    (<*>) = M.ap

throwError :: Monad m => e -> ExceptT e m a
throwError err = ExceptT $ return $ Left err


--- Dummy Monad instead of IO for Parser

newtype DummyMonad a = DummyMonad { runDummyMonad :: a }
instance Functor DummyMonad where
    fmap :: (a -> b) -> DummyMonad a -> DummyMonad b
    fmap f (DummyMonad a) = DummyMonad $ f a

instance Monad DummyMonad where
    return :: a -> DummyMonad a
    return a = DummyMonad a

    (>>=) :: DummyMonad a -> (a -> DummyMonad b) -> DummyMonad b
    (DummyMonad a) >>= f = f a

instance Applicative DummyMonad where
    pure  = return
    (<*>) = M.ap




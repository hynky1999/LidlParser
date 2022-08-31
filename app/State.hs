{-# LANGUAGE InstanceSigs #-}

module State where

import qualified Control.Monad                 as M

newtype State s a = State { runState :: s -> (s, a) }
-- Připomínám: runState :: State s a -> s -> (s, a)

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f stavovyVypocet = State $ \s ->       -- Nový stavový výpočet bere stav
        let (s', a) = runState stavovyVypocet s -- spustí starý výpočet, čímž dostane nový stav a ačko 
        in  (s', f a)                           -- a vrátí nový stav a 'f a :: b'

instance Monad (State s) where
    return :: a -> State s a
    return a = State $ \s -> (s, a)

    (>>=) :: State s a -> (a -> State s b) -> State s b
    stavovyVypocet >>= f = State $ \s ->
        let (s', a)         = runState stavovyVypocet s  -- spustíme starý výpočet, dostaneme nový stav a ačko
            stavovyVypocet' = f a                -- aplikací f na a dostaneme nový stavový výpočet
        in  runState stavovyVypocet' s'          -- který spustíme aplikací na nový stav

-- | Vrátí aktuální stav
get :: State s s
get = State $ \s -> (s, s)

-- | Nastaví nový stav
set :: s -> State s ()
set s = State $ const (s, ())

-- | Změní stav pomocí dodané funkce
modify :: (s -> s) -> State s ()
modify f = State $ \s -> (f s, ())

-- | Spustí stavový výpočet s počátečním stavem,
-- vrátí konečný stav.
evalState :: s -> State s a -> s
evalState initialState action = fst $ runState action initialState

-- | Tuto instanci potřebujeme, protože Applicative je nadtřída Monad.
-- Reálně ji ale můžeme definovat pomocí Monad samotné, tedy tak učiňme :)
instance Applicative (State s) where
    pure  = return
    (<*>) = M.ap

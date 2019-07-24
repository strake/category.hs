{-# LANGUAGE RankNTypes #-}

module Data.Functor.Trans.Reader where

import Control.Categorical.Functor
import Control.Categorical.Monad
import Data.Function (flip)

newtype ReaderT s r f a = ReaderT { runReaderT :: r `s` f a }

instance {-# INCOHERENT #-} Functor s t f => Functor s (->) (ReaderT t r f) where
    map f (ReaderT x) = ReaderT (map f . x)

instance (Functor t (->) f, Functor (->) (->) (s r)) => Functor t (->) (ReaderT s r f) where
    map f (ReaderT x) = ReaderT ((map f :: _ -> _) <$> x)

instance Monad (->) f => Monad (->) (ReaderT (->) r f) where
    unit = ReaderT . unit . unit
    join (ReaderT x) = ReaderT (\ r -> (flip id r >=> flip runReaderT r) x)

instance Comonad (->) ɯ => Comonad (->) (ReaderT (,) r ɯ) where
    counit = counit . counit . runReaderT
    cut (ReaderT (r, x)) = ReaderT (r, cobind (ReaderT . (,) r) x)

instance (Functor t (->) (s r)) => Functor (NT t) (NT (->)) (ReaderT s r) where
    map f = NT (\ (ReaderT x) -> ReaderT (nt f <$> x))

instance Monad (->) (s r) => Monad (NT (->)) (ReaderT s r) where
    unit = NT (ReaderT . unit)
    join = NT (ReaderT . bind runReaderT . runReaderT)

instance Comonad (->) (s r) => Comonad (NT (->)) (ReaderT s r) where
    counit = NT (counit . runReaderT)
    cut = NT (ReaderT . cobind ReaderT . runReaderT)

instance Functor t (NT (->)) s => Functor t (NT (NT (->))) (ReaderT s) where
    map f = NT (NT (\ (ReaderT x) -> ReaderT (nt (map f) x)))

module Data.Functor.Trans.Writer where

import Control.Categorical.Functor
import Control.Categorical.Monad

newtype WriterT p w f a = WriterT { runWriterT :: f (p w a) }

instance (Functor (->) (->) f, Functor s (->) (p w)) => Functor s (->) (WriterT p w f) where
    map f (WriterT x) = WriterT ((map f :: _ -> _) <$> x)

instance (Monoid w, Monad (->) f) => Monad (->) (WriterT (,) w f) where
    unit = WriterT . unit . unit
    join = WriterT . bind (\ (w, WriterT y) -> map (\ (w', a) -> (w <> w', a)) y) . runWriterT

instance (Comonad (->) (p w), Comonad (->) f) => Comonad (->) (WriterT p w f) where
    counit = counit . counit . runWriterT
    cut = WriterT . cobind (\ x -> (\ _ -> WriterT x) <$> counit x) . runWriterT

instance Monad (->) f => Monad (->) (WriterT Either w f) where
    unit = WriterT . unit . unit
    join = WriterT . bind (\ case Left w -> unit (Left w)
                                  Right (WriterT x) -> x) . runWriterT

instance Functor (NT (->)) (NT (->)) (WriterT p w) where
    map f = NT (\ (WriterT x) -> WriterT (nt f x))

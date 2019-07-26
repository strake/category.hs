module Data.Functor.Trans.Identity where

import Control.Categorical.Functor
import Control.Categorical.Monad

newtype IdentityT f a = IdentityT { runIdentityT :: f a }

instance Functor (NT (->)) (NT (->)) IdentityT where
    map f = NT (\ (IdentityT x) -> IdentityT (nt f x))

instance Monad (->) m => Functor (NT (Kleisli (->) m)) (NT (Kleisli (->) m)) IdentityT where
    map f = NT (Kleisli (map IdentityT . kleisli (nt f) . runIdentityT))

instance Comonad (->) ɯ => Functor (NT (Cokleisli (->) ɯ)) (NT (Cokleisli (->) ɯ)) IdentityT where
    map f = NT (Cokleisli (IdentityT . cokleisli (nt f) . map runIdentityT))

instance Monad (NT (->)) IdentityT where
    unit = NT IdentityT
    join = NT runIdentityT

instance Comonad (NT (->)) IdentityT where
    counit = NT runIdentityT
    cut = NT IdentityT

instance Monad (->) m => Monad (NT (Kleisli (->) m)) IdentityT where
    unit = NT (Kleisli (unit . IdentityT))
    join = NT (Kleisli (unit . runIdentityT))

instance Comonad (->) ɯ => Monad (NT (Cokleisli (->) ɯ)) IdentityT where
    unit = NT (Cokleisli (IdentityT . counit))
    join = NT (Cokleisli (runIdentityT . counit))

instance Comonad (->) ɯ => Comonad (NT (Cokleisli (->) ɯ)) IdentityT where
    counit = NT (Cokleisli (runIdentityT . counit))
    cut = NT (Cokleisli (IdentityT . counit))

instance Monad (->) m => Comonad (NT (Kleisli (->) m)) IdentityT where
    counit = NT (Kleisli (unit . runIdentityT))
    cut = NT (Kleisli (unit . IdentityT))

deriving instance Functor s (->) f => Functor s (->) (IdentityT f)

instance Monad (->) f => Monad (->) (IdentityT f) where
    unit = IdentityT . unit
    join = IdentityT . bind runIdentityT . runIdentityT

instance Comonad (->) f => Comonad (->) (IdentityT f) where
    counit = counit . runIdentityT
    cut = runIdentityT . cobind runIdentityT . IdentityT

instance (Functor s (Kleisli (->) m) f, Endofunctor (->) m) =>
         Functor s (Kleisli (->) m) (IdentityT f) where
    map f = Kleisli (map IdentityT . kleisli (map f) . runIdentityT)

instance (Functor s (Cokleisli (->) ɯ) f, Endofunctor (->) ɯ) =>
         Functor s (Cokleisli (->) ɯ) (IdentityT f) where
    map f = Cokleisli (IdentityT . cokleisli (map f) . map runIdentityT)

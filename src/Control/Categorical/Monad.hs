{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
module Control.Categorical.Monad (Monad (..), (<=<), (>=>), Kleisli (..), Comonad (..), (=<=), (=>=), Cokleisli (..)) where

import qualified Control.Applicative as Base
import qualified Control.Monad as Base
import Control.Monad.Trans.Identity (IdentityT (..))
import Data.Function (($), flip)
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Arg (..))
import qualified Data.Traversable as Base

import Control.Categorical.Functor
import Control.Category.Dual

infixr 1 >=>, <=<, =>=, =<=

class Endofunctor s m => Monad s m where
    unit :: a `s` m a

    join :: m (m a) `s` m a
    join = bind id

    bind :: a `s` m b -> m a `s` m b
    bind f = join . map f

(<=<) :: Monad s m => b `s` m c -> a `s` m b -> a `s` m c
f <=< g = bind f . bind g . unit

(>=>) :: Monad s m => a `s` m b -> b `s` m c -> a `s` m c
(>=>) = flip (<=<)

newtype Kleisli s m a b = Kleisli { kleisli :: a `s` m b }

instance Monad s m => Category (Kleisli s m) where
    id = Kleisli unit
    Kleisli f . Kleisli g = Kleisli (f <=< g)

instance {-# INCOHERENT #-} Base.Monad m => Monad (->) m where
    unit = Base.return
    join = Base.join
    bind = (Base.=<<)

instance Comonad s f => Monad (Dual s) f where
    unit = Dual counit
    join = Dual cut

instance (Category s, Comonad (NT s) f) => Monad (NT (Dual s)) f where
    unit = NT (Dual (nt counit))
    join = NT (Dual (nt cut))

class Endofunctor s ɯ => Comonad s ɯ where
    counit :: ɯ a `s` a

    cut :: ɯ a `s` ɯ (ɯ a)
    cut = cobind id

    cobind :: ɯ a `s` b -> ɯ a `s` ɯ b
    cobind f = map f . cut

(=<=) :: Comonad s ɯ => ɯ b `s` c -> ɯ a `s` b -> ɯ a `s` c
f =<= g = counit . cobind f . cobind g

(=>=) :: Comonad s ɯ => ɯ a `s` b -> ɯ b `s` c -> ɯ a `s` c
(=>=) = flip (=<=)

newtype Cokleisli s ɯ a b = Cokleisli { cokleisli :: ɯ a `s` b }

instance Comonad s ɯ => Category (Cokleisli s ɯ) where
    id = Cokleisli counit
    Cokleisli f . Cokleisli g = Cokleisli (f =<= g)

instance Comonad (->) Identity where
    counit = runIdentity
    cut = map Identity

instance Comonad (->) NonEmpty where
    counit = NE.head
    cut (x:|xs) = (x:|xs) :| go xs
      where go [] = []
            go (x:xs) = (x:|xs) : go xs

instance Monoid m => Comonad (->) ((->) m) where
    counit = ($ mempty)
    cut f x y = f (x <> y)

instance Comonad (->) ((,) a) where
    counit (_, b) = b
    cut (a, b) = (a, (a, b))

instance Comonad (->) (Arg a) where
    counit (Arg _ b) = b
    cut (Arg a b) = Arg a (Arg a b)

instance Functor s t m => Functor s (->) (Kleisli t m a) where
    map f (Kleisli φ) = Kleisli (map f . φ)

instance Category s => Functor s (->) (Cokleisli s ɯ a) where
    map f (Cokleisli φ) = Cokleisli (f . φ)

instance Category s => Functor (Dual s) (NT (->)) (Kleisli s m) where
    map (Dual f) = NT (\ (Kleisli φ) -> Kleisli (φ . f))

instance Functor s t ɯ => Functor (Dual s) (NT (->)) (Cokleisli t ɯ) where
    map (Dual f) = NT (\ (Cokleisli φ) -> Cokleisli (φ . map f))

instance Monad s m => Functor (Kleisli s m) s m where
    map = bind . kleisli

instance Comonad s ɯ => Functor (Cokleisli s ɯ) s ɯ where
    map = cobind . cokleisli

instance {-# OVERLAPPABLE #-} (Base.Traversable f, Monad (->) m) => Functor (Kleisli (->) m) (Kleisli (->) m) f where
    map (Kleisli f) = Kleisli (unBaseMonad . Base.traverse (BaseMonad . f))

data BaseMonad m a = Monad (->) m => BaseMonad { unBaseMonad :: m a }
instance Base.Functor (BaseMonad f) where fmap = map
instance Base.Applicative (BaseMonad m) where pure = unit; (<*>) = Base.ap
instance Base.Monad (BaseMonad m) where (>>=) = flip bind

instance Monad (->) m => Functor (NT (Kleisli (->) m)) (NT (Kleisli (->) m)) IdentityT where
    map f = NT (Kleisli (map IdentityT . kleisli (nt f) . runIdentityT))

instance Monad (Dual (->)) m => Functor (NT (Kleisli (Dual (->)) m)) (NT (Kleisli (Dual (->)) m)) IdentityT where
    map f = NT (Kleisli (Dual (IdentityT . dual (kleisli (nt f)) . dual (map (Dual runIdentityT)))))

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

instance (Monad (->) m) => Functor (Kleisli (->) m) (Kleisli (->) m) ((,) a) where
    map (Kleisli f) = Kleisli (\ (a, b) -> (,) a <$> f b)

instance (Monad (->) m) => Functor (Kleisli (->) m) (NT (Kleisli (->) m)) (,) where
    map (Kleisli f) = NT (Kleisli (\ (a, b) -> (\ a -> (a, b)) <$> f a))

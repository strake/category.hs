{-# LANGUAGE RankNTypes #-}

module Control.Categorical.Functor where

import Control.Category.Dual
import Control.Category.Groupoid
import qualified Data.Functor as Base
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Const
import Data.Functor.Product
import Data.Functor.Sum
import Data.Proxy

-- | Laws:
--
-- @
-- 'map' 'id' = 'id'
-- 'map' (f '.' g) = 'map' f '.' 'map' g
-- @
class (Category s, Category t) => Functor (s :: α -> α -> *) (t :: β -> β -> *) (f :: α -> β) where
    map :: s a b -> t (f a) (f b)

{-# DEPRECATED EndoFunctor "Use Endofunctor" #-}
type EndoFunctor s = Functor s s
type Endofunctor s = Functor s s

infixl 4 <$>
(<$>) :: Functor s (->) f => s a b -> f a -> f b
(<$>) = map

newtype NT s f g = NT { nt :: ∀ a . s (f a) (g a) }

instance Category s => Category (NT s) where
    id = NT id
    NT f . NT g = NT (f . g)

instance Groupoid s => Groupoid (NT s) where
    invert (NT f) = NT (invert f)

instance {-# INCOHERENT #-} Base.Functor f => Functor (->) (->) f where map = Base.fmap

instance Functor s (->) f => Functor (NT s) (NT (->)) (Compose f) where
    map (NT f) = NT (\ (Compose x) -> Compose (f <$> x))

instance Functor (NT (->)) (NT (NT (->))) Compose where
    map (NT f) = NT (NT (\ (Compose x) -> Compose (f x)))

instance (Functor s (->) f, Functor s (->) g) => Functor s (->) (Sum f g) where
    map f (InL x) = InL (f <$> x)
    map f (InR y) = InR (f <$> y)

instance Functor (NT (->)) (NT (->)) (Sum f) where
    map (NT f) = NT (\ case InL x -> InL x
                            InR y -> InR (f y))

instance Functor (NT (->)) (NT (NT (->))) Sum where
    map (NT f) = NT (NT (\ case InL x -> InL (f x)
                                InR y -> InR y))

instance (Functor s (->) f, Functor s (->) g) => Functor s (->) (Product f g) where
    map f (Pair x y) = Pair (f <$> x) (f <$> y)

instance Functor (NT (->)) (NT (->)) (Product f) where
    map (NT f) = NT (\ (Pair x y) -> Pair x (f y))

instance Functor (NT (->)) (NT (NT (->))) Product where
    map (NT f) = NT (NT (\ (Pair x y) -> Pair (f x) y))

instance Category s => Functor s (->) (Const a) where
    map _ (Const a) = Const a

instance Functor (->) (NT (->)) Const where
    map f = NT (\ (Const a) -> Const (f a))

instance Functor (->) (->) Identity where
    map f (Identity a) = Identity (f a)

instance Category s => Functor s (->) Proxy where
    map _ Proxy = Proxy

instance Functor (->) (->) ((,) a) where
    map f (a, b) = (a, f b)

instance Functor (->) (NT (->)) (,) where
    map f = NT (\ (a, b) -> (f a, b))

instance Functor (->) (->) (Either a) where
    map _ (Left a) = Left a
    map f (Right b) = Right (f b)

instance Functor (->) (NT (->)) Either where
    map f = NT (\ case Left a -> Left (f a)
                       Right b -> Right b)

instance Category s => Functor s (->) (s a) where
    map = (.)

instance Category s => Functor (Dual s) (NT (->)) s where
    map (Dual f) = NT (. f)

instance Functor s t f => Functor (Dual s) (Dual t) f where
    map (Dual f) = Dual (map f)

instance (Category t, Functor s (NT t) f) => Functor (Dual s) (NT (Dual t)) f where
    map (Dual f) = NT (Dual (nt (map f)))

instance (Category s, Category t, Functor s (NT (Dual t)) f) => Functor s (Dual (NT t)) f where
    map f = Dual (NT (dual (nt (map f))))

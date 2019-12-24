module Control.Category.Const2 where

import Algebra as A
import Control.Categorical.Functor
import Control.Categorical.Monad
import Control.Category.Groupoid
import Relation.Binary.Comparison

-- | Notes: 'Const2' '()' is the indiscrete category.
newtype Const2 a b c = Const2 { getConst2 :: a }
  deriving (Semigroup, Monoid, Group, PartialEq, Preord, Eq, PartialOrd, Ord) via a

instance Functor (->) (NT (NT (->))) Const2 where
    map f = NT (NT (\ (Const2 a) -> Const2 (f a)))

instance Monad (->) m => Functor (Kleisli (->) m) (NT (NT (Kleisli (->) m))) Const2 where
    map (Kleisli f) = NT (NT (Kleisli (\ (Const2 a) -> Const2 <$> f a)))

instance (Semigroup a, Monoid a) => Category (Const2 a) where
    id = Const2 mempty
    Const2 a . Const2 b = Const2 (a <> b)

instance (Semigroup a, Group a) => Groupoid (Const2 a) where
    invert (Const2 a) = Const2 (A.invert a)

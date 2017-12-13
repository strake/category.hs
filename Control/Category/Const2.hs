module Control.Category.Const2 where

import Algebra as A
import Control.Category.Groupoid

newtype Const2 a b c = Const2 a
  deriving (Semigroup, Monoid, Group)

instance (Semigroup a, Monoid a) => Category (Const2 a) where
    id = Const2 mempty
    Const2 a . Const2 b = Const2 (a <> b)

instance (Semigroup a, Group a) => Groupoid (Const2 a) where
    invert (Const2 a) = Const2 (A.invert a)

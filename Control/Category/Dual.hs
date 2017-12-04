module Control.Category.Dual where

import Control.Category.Groupoid

newtype Dual k a b = Dual { dual :: k b a }
  deriving (Semigroup, Monoid, Group)

instance Category k => Category (Dual k) where
    id = Dual id
    Dual f . Dual g = Dual (g . f)

instance Groupoid k => Groupoid (Dual k) where
    invert = Dual . invert . dual

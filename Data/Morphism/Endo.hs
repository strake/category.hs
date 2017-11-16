module Data.Morphism.Endo where

import Algebra as A
import Control.Category.Groupoid as C

newtype Endo s a = Endo { endo :: s a a }

instance Category s => Semigroup (Endo s a) where
    Endo f <> Endo g = Endo (f . g)

instance Category s => Monoid (Endo s a) where
    mempty = Endo id

instance Groupoid s => Group (Endo s a) where
    invert (Endo f) = Endo (C.invert f)

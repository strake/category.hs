module Data.Morphism.Iso where

import Control.Categorical.Functor
import Control.Category.Dual
import Control.Category.Groupoid

data Iso s a b = Iso (s a b) (s b a)

instance Category s => Category (Iso s) where
    id = Iso id id
    Iso f f' . Iso g g' = Iso (f . g) (g' . f')

instance Category s => Groupoid (Iso s) where
    invert (Iso f f') = Iso f' f

instance Functor s t f => Functor (Iso s) t f where
    map (Iso f _) = map f

instance Functor s t f => Functor (Iso s) (Dual t) f where
    map (Iso _ f') = Dual (map f')

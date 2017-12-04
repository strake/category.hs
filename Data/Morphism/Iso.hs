module Data.Morphism.Iso where

import qualified Algebra as A
import Control.Categorical.Functor
import Control.Category.Dual
import Control.Category.Groupoid

data Iso s a b = Iso (s a b) (s b a)

instance (Semigroup (s a b), Semigroup (s b a)) => Semigroup (Iso s a b) where
    Iso f f' <> Iso g g' = Iso (f <> g) (f' <> g')

instance (Monoid (s a b), Monoid (s b a)) => Monoid (Iso s a b) where
    mempty = Iso mempty mempty

instance (Group (s a b), Group (s b a)) => Group (Iso s a b) where
    invert (Iso f f') = Iso (A.invert f) (A.invert f')

instance Category s => Category (Iso s) where
    id = Iso id id
    Iso f f' . Iso g g' = Iso (f . g) (g' . f')

instance Category s => Groupoid (Iso s) where
    invert (Iso f f') = Iso f' f

instance Functor s t f => Functor (Iso s) t f where
    map (Iso f _) = map f

instance Functor s t f => Functor (Iso s) (Dual t) f where
    map (Iso _ f') = Dual (map f')

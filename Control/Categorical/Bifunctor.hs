{-# LANGUAGE TypeOperators #-}

module Control.Categorical.Bifunctor where

import qualified Data.Bifunctor as Base

import Control.Categorical.Functor
import Control.Category.Dual
import Data.Functor.Product
import Data.Functor.Sum

class (Functor r (NT t) f, Category s) => Bifunctor r s t f where
    bimap :: r aᵣ bᵣ -> s aₛ bₛ -> f aᵣ aₛ `t` f bᵣ bₛ

infixr 3 <***>, <⁂>
(<***>), (<⁂>) :: Bifunctor r s t f => r aᵣ bᵣ -> s aₛ bₛ -> f aᵣ aₛ `t` f bᵣ bₛ
(<***>) = bimap
(<⁂>)   = bimap

instance {-# INCOHERENT #-} Base.Bifunctor f => Bifunctor (->) (->) (->) f where
    bimap = Base.bimap

instance Category s => Bifunctor (Dual s) s (->) s where
    bimap (Dual f) g a = g . a . f

instance Bifunctor (NT (->)) (NT (->)) (NT (->)) Product where
    bimap (NT f) (NT g) = NT (\ (Pair x y) -> Pair (f x) (g y))

instance Bifunctor (NT (->)) (NT (->)) (NT (->)) Sum where
    bimap (NT f) (NT g) = NT (\ case InL x -> InL (f x)
                                     InR y -> InR (g y))

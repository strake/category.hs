module Control.Category.Groupoid where

import Control.Category.Dual

-- | 'Category' where every morphism is iso
--
-- Laws:
--
-- @
-- 'id' = f '.' 'invert' f
-- 'id' = 'invert' f '.' f
-- @
class Category k => Groupoid k where
    invert :: k a b -> k b a

instance Groupoid k => Groupoid (Dual k) where invert = Dual . invert . dual

module Control.Category.Groupoid where

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

module Control.Category.Groupoid where

class Category k => Groupoid k where
    invert :: k a b -> k b a

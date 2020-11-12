module Main where

import Prelude hiding ( Maybe, Nothing, Just
                      , Either, Left, Right
                      )

data Maybe a = Nothing
             | Just a
             deriving (Show, Eq)

instance Functor Maybe where
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)

instance Applicative Maybe where
    pure = Just

    (<*>) Nothing _ = Nothing
    (<*>) _ Nothing = Nothing
    (<*>) (Just f) (Just x) = Just (f x)

instance Alternative Maybe where
    Nothing <|> r = r
    l       <|> _ = l


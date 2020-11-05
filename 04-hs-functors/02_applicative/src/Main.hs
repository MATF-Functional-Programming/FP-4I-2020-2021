module Main where

import Prelude hiding ( Maybe, Nothing, Just
                      , Either, Left, Right
                      )
import qualified Data.Char as Ch
import Debug.Trace (trace)
import Data.Bifunctor

main :: IO ()
main = do
  putStrLn "hello world"


data Maybe a = Nothing
             | Just a
             deriving (Show, Eq)

instance Functor Maybe where
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)

instance Applicative Maybe where
    -- x -> Maybe x
    pure x = Just x

    (<*>) Nothing _ = Nothing
    (<*>) _ Nothing = Nothing
    (<*>) (Just f) (Just x) = Just (f x)


maybeResult = (Just (+)) <*> (Just 42) <*> (Nothing)




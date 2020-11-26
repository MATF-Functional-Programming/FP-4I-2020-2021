import Data.Char
import Data.Monoid
import Data.Foldable

-- Assume we want to filter some messages if they contain more than n forbidden characters

isBraille :: Char -> Bool
isBraille = undefined

isEmoji :: Char -> Bool
isEmoji = undefined

-- How to combine these predicates into a complex predicate?
-- combinePredicates :: (Char -> Bool) -> (Char -> Bool) -> Char -> Bool

-- s1mple, but not scalable
isForbidden' :: Char -> Bool
isForbidden' c = isBraille c || isEmoji c      -- || ... 

-- better
isForbidden'' :: Char -> Bool
isForbidden'' c = any (\p -> p c) predicates
    where predicates = [isBraille, isEmoji]    -- add predicates here in future

-- Monoid solution: combinePredicates is `mappend`

-- doesnt compile because (Char -> Bool) is not monoid, since Bool is not Monoid
-- (check `:i (->)`)
-- isForbidden''' :: Char -> Bool
-- isForbidden''' c = fold predicates
--     where predicates = [isBraille, isEmoji]


-- we can use Any or All which are Bool wrappers but are 
-- disjunctive/conjunctive Monoids
isForbidden''' :: Char -> Bool
isForbidden''' c = getAny $ fold $ map (Any.) predicates
    where predicates = [isBraille, isEmoji]

-- or shorter:
isForbidden'''' :: Char -> Bool
isForbidden'''' c = getAny . foldMap (Any.) predicates
    where predicates = [isBraille, isEmoji]

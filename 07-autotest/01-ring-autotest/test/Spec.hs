import Lib

import Data.Function (on)

-- import bib za testiranje, dodati - QuickCheck u deps
import Test.QuickCheck

main :: IO ()
main = putStrLn "Test suite not yet implemented"

-- Da biste ucitali testove u GHCi, pokrenite ghci na sledeci nacin:
-- stack ghci ring-delist:ring-delist-test

-- Pojedinacne testove mozete da pokrecete sa:
-- quickCheck ime_testa
-- quickCheck (withMaxSuccess 10000 ime_testa)

prop_toList_fromList_conv s =
            s == (toList $ fromList s)
            where types = (s :: [Int])

prop_focusNext_focusPrev s =
            s == (toList $ focusPrev $ focusNext $ fromList s)
            where types = (s :: [Int])

prop_focusPrev_focusNext :: [Int] -> Bool
prop_focusPrev_focusNext s =
            s == (toList $ focusNext $ focusPrev $ fromList s)

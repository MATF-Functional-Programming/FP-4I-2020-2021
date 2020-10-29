import Data.Char

-----------------------------------------------------------------------------
-- Fold (reduce, accumulate) ------------------------------------------------
-----------------------------------------------------------------------------

-- Obicna rekurzija
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

-- Repna rekurzija -- u svakom pozivu prosledjujemo
-- prethodnu akumuliranu vrednost
mySum' :: Num a => [a] -> a
mySum' xs =
    let mySumHelper acc values =
                    case values of
                         [] -> acc
                         (x:xs) -> mySumHelper (acc + x) xs
    in mySumHelper 0 xs

-- Fold je genericki algoritam koji implementira
-- repnu rekurziju
mySum'' xs =
    foldl (+) 0 xs

-- Filtriranje rekurzijom
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs) = let filteredTail = myFilter p xs
                    in if p x then x: filteredTail
                              else filteredTail

-- Filtriranje repnom rekurzijom
myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' _ [] = []
myFilter' p xs =
    let myFilterHelper acc values =
                       case values of
                            [] -> acc
                            (x:xs) -> myFilterHelper (if p x then x:acc else acc) xs
    in myFilterHelper [] (reverse xs)

-- Filtriranje kroz fold
myFilter'' :: (a -> Bool) -> [a] -> [a]
myFilter'' p xs =
    foldr (\ x acc -> if p x then x:acc else acc) [] xs


-- Implementacija konkatenacije preko folda
myConcat :: [a] -> [a] -> [a]
myConcat ls rs =
    foldr (:) rs ls


-- Reverse na nekoliko nacina
reverseRec :: [a] -> [a]
reverseRec [] = []
reverseRec (x:xs) = reverseRec xs ++ [x]

reverseTail :: [a] -> [a]
reverseTail lst = let reverseHelper acc lst
                        | []     <- lst     = acc
                        | (x:xs) <- lst     = reverseHelper (x:acc) xs
                  in reverseHelper [] lst 

reverseFoldr :: [a] -> [a]
reverseFoldr lst = foldr (\ x acc -> acc ++ [x]) [] lst

reverseFoldl :: [a] -> [a]
reverseFoldl lst = foldl (flip (:)) [] lst


-- Map preko folda
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\ x acc -> f x : acc) [] xs


--
-- Ekstraktujemo jednu rec iz recenice
--

-- ne ignorise praznine na početku stringa
takeWord' :: String -> String
takeWord' sentence = takeWhile (not . isSpace) sentence

-- ignorišemo praznine, ali nije mnogo čitljivo rešenje
takeWord'' :: String -> String
takeWord'' sentence = takeWhile (not . isSpace) (dropWhile isSpace sentence)

-- pointfree verzija funkcije
takeWord :: String -> String
takeWord = takeWhile (not . isSpace) . dropWhile isSpace

--
takeWordVerbose :: String -> String
takeWordVerbose =
        let isNotSpace = not . isSpace
        in  takeWhile isNotSpace . dropWhile isSpace



--
-- Ekstraktujemo prve dve reci iz recenice
--

-- ako želimo da ekstraktujemo prve dve reči, moramo da
-- zapamtimo ostatak stringa posle izvlačenja prve reči
breakWord :: String -> (String, String)
breakWord = break isSpace . dropWhile isSpace

-- možemo da spajamo pozive prethodne funkcije koliko
-- god puta želimo (@see Monade)
breakWord2 :: String -> (String, String, String)
breakWord2 sentence =
        let (word1, rest1) = breakWord sentence
            (word2, rest2) = breakWord rest1
        in  (word1, word2, rest2)

-- postoje ugradjene f-je koje razbijaju stringove
-- na linije i reči, kao i njihove inverzne f-je:
-- words, lines, unwords, unlines



--
-- Imamo listu, hoćemo da je filtriramo u odnosu
-- na indekse elemenata
--

filterByIndex p xs =
    let withIndices xs = zip [1..] xs    -- zipujemo xs sa beskonačnom
        indexed        = withIndices xs  -- listom celih brojeva
        pairpred       = p . fst         -- primenjujemo predikat na prvu komponentu redjenog para
    in map snd $ filter pairpred indexed


--
-- Želimo da napravimu f-ju koja računa vrednosti liste
-- f-ja u odredjenoj tački
-- applyFunctions (pi/3) [sin, cos, tan, atan]
--

applyFunctions'  x fns = map (\ f -> f x) fns
applyFunctions   x fns = map ($x) fns
applyFunctionsPF       = map . flip id



-- Cuvari (guards) su lepsi zapis za lanac if else if else if naredbi
accumulate :: (Eq a, Num a) => (a -> a -> a) -> [a] -> a
accumulate f xs
        | xs == []   = 0
        | otherwise  = f (head xs) (accumulate f $ tail xs)

-- Mogu da sadrze i pattern matching izraze
-- (linije koje sadrze <- xs)
accumulate' :: (Eq a, Num a) => (a -> a -> a) -> [a] -> a
accumulate' f xs
        | xs == []       = 0
        | (x:[])  <- xs  = x
        | (x:xs)  <- xs  = f x (accumulate' f xs)


-- Quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort bigger
                    where smaller = [e | e <- xs, e < x]
                          bigger = [e | e <- xs, e >= x]


main :: IO ()
main = putStrLn "Hello"




import System.Environment

import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap

import Data.Tuple

-- IO a - omotacki tip koji predstavlja runtime akciju
--        koja vraca/generise instancu tipa a

main :: IO ()
main = do
    -- Učitavamo stdin u listu
    text <- getContents -- IO String

    -- Učitavamo prvi argument komandne linije, ostale ignorišemo
    -- (ne proveravamo da li argument postoji)
    (arg: _) <- getArgs

    -- Konvertujemo argument u broj
    let n = read arg :: Int

    -- Izlazimo iz 'nečistog' dela programa
    putStrLn $ process n text


process :: Int -> String -> String
process n text =
    let -- Uklanjamo neželjene karaktere, i delimo ulaz na reči
        ws = words $ map Char.toLower $
                     map (\ c -> if Char.isLetter c then c else ' ') $ text

        -- Računamo broj pojavljivanja svake reči
        word_occs = HashMap.toList $
                    HashMap.fromListWith (+) [ (w, 1) | w <- ws ]

        -- Sortiramo listu po broju pojavljivanja reči
        sorted_by_occs = List.sortBy (flip compare) $ map swap $ word_occs

    -- Ispisujemo prvih n rezultata
    in unlines $ map show $ take n $ sorted_by_occs



module Zad3 where

-- Zadatak 3:

-- Definisati strukturu podataka `Stek a`, gde je `a`
-- proizvoljan tip. Stek moze biti prazan ili sadrzati
-- proizvoljan broj elemenata tipa `a`.

-- Kreirati funkcije `fromList` i `toList` koje konvertuju
-- [a] u `Stek a`, i `Stek a` u `[a]`, redom.

-- Napisati QuickCheck test za funkcije `fromList` i `toList`.

-- Instancirati Show nad `Stek a` (pretpostaviti da je
-- `a` u klasi Show) tako da se skup ispise u formatu:
--       < dno, ... , vrh >
-- (vrh i dno su elementi steka)
-- Za prazan stek:  </>

-- Instancirati Functor nad `Stek a`

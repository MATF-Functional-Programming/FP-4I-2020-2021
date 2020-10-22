module Lib where

-- Zelimo da napravimo jedan deo window menadzera
-- (program koji kontrolise prozore na Linux/X11 sistemima)

-- Glavni posao WM-a je da prati koji prozori postoje,
-- koji je prozor trenutno aktivan. i da prebacuje
-- fokus sa prozora na prozor (alt+tab).
--
-- Kad se jednom alt+tab pritisne, prelazi se na sledeci
-- prozor. Kad se pritisne sledeceg puta, prelazi se
-- na prethodno aktivan prozor.
--
-- Ako se pritiska tab vise puta dok se drzi pritisnut
-- alt, prolazi se ciklicno kroz sve prozore.
--
-- Za sve ovo treba implementirati funkcije.
--
-- Prozore cemo cuvati u listi. Prozor na vrhu liste
-- smatramo da je aktivan


-- Funkcija koja aktivira sledeci prozor (jedan alt+tab)
-- dobija trenutnu listu prozora, a vraca novu listu prozora
-- u kojoj ce novi prozor biti aktiviran (na vrhu liste).
-- Prethodno aktivan prozor ide na drugo mesto u listi
switchActive :: [t] -> [t]
switchActive []  = []  -- nema prozora
switchActive [w] = [w] -- samo jedan prozor, nemamo sta da promenimo
switchActive (active:next:others) = next:active:others


-- Funkcija koja simulira 'povezani alt+tab' ciklicno prolazi
-- kroz sve prozore, sto znaci da trenutno aktivni prozor ide
-- na kraj liste pri svakom alt+tab-u.
focusNext :: [t] -> [t]
focusNext []     = []
focusNext (x:xs) = xs ++ [x] -- ++ je operator za konkat. lista

-- shift+alt+tab prolazi kroz prozore u obrnutom redosledu.
-- Moze da se implementira jednostavnije i efikasnije, ali
-- radi demonstracije kompozicije funkcija...:
focusPrev :: [t] -> [t]
focusPrev = reverse . focusNext . reverse


-- Zatvaranje (alt+f4) prozora zatvara trenutno aktivan prozor,
-- to jest, brise glavu liste
closeWindow :: [t] -> [t]
closeWindow = tail


-- Kad se prozor otvori, postaje aktivan i dolazi na vrh liste
openWindow :: t -> [t] -> [t]
openWindow = (:)
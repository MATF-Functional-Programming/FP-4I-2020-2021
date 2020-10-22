module Lib where

-- [...] Nastavak
--
-- U prethodnoj verziji nismo sakrili cinjenicu da nam je
-- interna reprezentacija obicna lista

-- Definisimo tip Ring t koji ce videti korisnici nase
-- biblioteke i koji cemo kasnije moci da promenimo a
-- da ne menjamo API nase biblioteke
type Ring t = [t]
-- type definise alias za tip nalik typedef i using u C-u i C++-u


-- Posto ne zelimo da korisnik se oslanja na nasu internu
-- reprezentaciju, moramo da mu dozvolimo da na neki nacin
-- napravi instancu tipa Ring, kao i da Ring konvertuje u
-- neki tip koji on razume.
--
-- Najlakse nam je da dozvolimo konverzije iz i u liste
fromList :: [t] -> Ring t
fromList = id

toList :: Ring t -> [t]
toList = id


-- Ispod su sve funkcije koje smo imali u prethodnom primeru
-- neizmenjene


-- Funkcija koja aktivira sledeci prozor (jedan alt+tab)
-- dobija trenutnu listu prozora, a vraca novu listu prozora
-- u kojoj ce novi prozor biti aktiviran (na vrhu liste).
-- Prethodno aktivan prozor ide na drugo mesto u listi
switchActive :: Ring t -> Ring t
switchActive []  = []  -- nema prozora
switchActive [w] = [w] -- samo jedan prozor, nemamo sta da promenimo
switchActive (active:next:others) = next:active:others


-- Funkcija koja simulira 'povezani alt+tab' ciklicno prolazi
-- kroz sve prozore, sto znaci da trenutno aktivni prozor ide
-- na kraj liste pri svakom alt+tab-u.
focusNext :: Ring t -> Ring t
focusNext []     = []
focusNext (x:xs) = xs ++ [x] -- ++ je operator za konkat. lista

-- shift+alt+tab prolazi kroz prozore u obrnutom redosledu.
-- Moze da se implementira jednostavnije i efikasnije, ali
-- radi demonstracije kompozicije funkcija...:
focusPrev :: Ring t -> Ring t
focusPrev = reverse . focusNext . reverse


-- Zatvaranje (alt+f4) prozora zatvara trenutno aktivan prozor,
-- to jest, brise glavu liste
closeWindow :: Ring t -> Ring t
closeWindow = tail


-- Kad se prozor otvori, postaje aktivan i dolazi na vrh liste
openWindow :: t -> Ring t -> Ring t
openWindow = (:)



module Lib where

-- [...] Nastavak

-- U prethodnoj verziji smo koristili uredjeni par.
-- U ovakvim slucajevima je bolje napraviti prave tipove
-- nego koristiti parove i torke
--
-- klucna rec 'data' pravi novi algebarski tip
-- (na sledecem casu detaljnije o algebarskim tipovima)
data Ring t = MkRing [t] [t]

-- Svuda gde smo koristili (,) sintaksu, sad treba da koristimo
-- MkRing


-- kad pravimo Ring, podrazumevano desna lista je prazna
fromList :: [t] -> Ring t
fromList xs = MkRing xs []

-- kad konvertujemo Ring u listu, moramo da spojimo
-- obe liste, samo drugu moramo da obrnemo prvo
toList :: Ring t -> [t]
toList (MkRing ls rs) = ls ++ reverse rs


-- Funkcija koja aktivira sledeci prozor (jedan alt+tab)
-- dobija trenutnu listu prozora, a vraca novu listu prozora
-- u kojoj ce novi prozor biti aktiviran (na vrhu liste).
-- Prethodno aktivan prozor ide na drugo mesto u listi
switchActive :: Ring t -> Ring t
switchActive (MkRing [] [])  = MkRing [] []  -- nema prozora
switchActive (MkRing [l] []) = MkRing [l] [] -- samo jedan prozor
switchActive (MkRing [l] rs) = 
    switchActive $ MkRing (l : reverse rs) []
switchActive (MkRing (active:next:others) rs) =
     MkRing (next:active:others) rs


-- Funkcija koja simulira 'povezani alt+tab' ciklicno prolazi
-- kroz sve prozore, sto znaci da trenutno aktivni prozor ide
-- na kraj liste pri svakom alt+tab-u.
focusNext :: Ring t -> Ring t
focusNext (MkRing [] [])     = MkRing [] []
focusNext (MkRing [l] rs)    = focusNext $ 
    MkRing (l : reverse rs) []
focusNext (MkRing (l:ls) rs) = MkRing ls (l:rs)

-- shift+alt+tab prolazi kroz prozore u obrnutom redosledu.
-- Moze da se implementira jednostavnije i efikasnije, ali
-- radi demonstracije kompozicije funkcija...:
focusPrev :: Ring t -> Ring t
focusPrev (MkRing [] [])    = MkRing [] []
focusPrev (MkRing ls [])    = focusPrev $ MkRing [] (reverse ls)
focusPrev (MkRing ls (r:rs)) = MkRing (r:ls) rs


-- Zatvaranje (alt+f4) prozora zatvara trenutno aktivan prozor,
-- to jest, brise glavu liste
closeWindow :: Ring t -> Ring t
closeWindow (MkRing [] [])     = MkRing [] []
closeWindow (MkRing [w] rs)    = MkRing (reverse rs) []
closeWindow (MkRing (l:ls) rs) = MkRing ls rs


-- Kad se prozor otvori, postaje aktivan i dolazi na vrh liste
openWindow :: t -> Ring t -> Ring t
openWindow w (MkRing ls rs) = MkRing (w:ls) rs



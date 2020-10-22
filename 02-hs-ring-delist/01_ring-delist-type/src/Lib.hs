module Lib where

-- [...] Nastavak
--
-- U prethodnoj verziji smo koristili obicne liste i svaki
-- korak ciklicnog kretanja je bio slozenosti O(n).
--
-- Mozemo da probamo nesto pametnije -- da na drugi nacin
-- napravimo ciklicnu listu.
--
-- Pamticemo kao par lista -- leva lista ce se ponasati
-- kao i do sad -- cuvace prvih nekoliko prozora u istom
-- redosledu kao i do sada, a desna ce biti ostale prozore
-- u obrnutom redosledu.
--
--    aktivan prozor je i dalje
--    glava liste (leve)
--           |
--           v
--  __________    _______
-- /          \  /       \
--  4  3  2  1    7  6  5
--                ^
--                glava druge liste
--
-- Prelazak na sledeci aktivan prozor je samo uklanjanje
-- glave leve liste O(1) i dodavanje iste na pocetak
-- desne liste O(1)
--
--    aktivan prozor je i dalje
--    glava liste (leve)
--        |
--        v
--  _______    _________
-- /       \  /         \
--  4  3  2    1  7  6  5
--
-- Samo u retkim slucajevima moramo da prebacujemo vise
-- prozora odjednom iz jedne liste u drugu. Posto smo
-- rekli da je glava leve liste aktivan prozor, ne smemo
-- da dozvolimo da leva lista bude prazna ako ima prozora.
--
--  _    _______________
-- / \  /               \
--  4    3  2  1  7  6  5
--
-- U slucaju iznad ne smemo da prebacimo prozor 4 u
-- desnu listu jer bi onda leva lista ostala prazna.
-- Moramo da prebacimo prozore iz desne u levu
-- pre nego sto to pokusamo
--
--  ___________________
-- /                   \  /\
--  3  2  1  7  6  5  4
--
-- Tek onda mozemo da prebacujemo prozor.


type Ring t = ([t], [t])



-- kad pravimo Ring, podrazumevano desna lista je prazna
fromList :: [t] -> Ring t
fromList xs = (xs, [])

-- kad konvertujemo Ring u listu, moramo da spojimo
-- obe liste, samo drugu moramo da obrnemo prvo
toList :: Ring t -> [t]
toList (ls, rs) = ls ++ reverse rs



-- Funkcija koja aktivira sledeci prozor (jedan alt+tab)
-- dobija trenutnu listu prozora, a vraca novu listu prozora
-- u kojoj ce novi prozor biti aktiviran (na vrhu liste).
-- Prethodno aktivan prozor ide na drugo mesto u listi
switchActive :: Ring t -> Ring t
switchActive ([], [])  = ([], [])  -- nema prozora
switchActive ([l], []) = ([l], []) -- samo jedan prozor
switchActive ([l], rs) = switchActive (l : reverse rs, [])
switchActive (active:next:others, rs) = (next:active:others, rs)


-- Funkcija koja simulira 'povezani alt+tab' ciklicno prolazi
-- kroz sve prozore, sto znaci da trenutno aktivni prozor ide
-- na kraj liste pri svakom alt+tab-u.
focusNext :: Ring t -> Ring t
focusNext ([], [])   = ([], [])
focusNext ([l], rs)  = focusNext (l : reverse rs, [])
focusNext (l:ls, rs) = (ls, l:rs)

-- shift+alt+tab prolazi kroz prozore u obrnutom redosledu.
-- Moze da se implementira jednostavnije i efikasnije, ali
-- radi demonstracije kompozicije funkcija...:
focusPrev :: Ring t -> Ring t
focusPrev ([], [])   = ([], [])
focusPrev (ls, [])   = focusPrev ([], reverse ls) -- privremeno krsimo obecanje da je ls != []
focusPrev (ls, r:rs) = (r:ls, rs)


-- Zatvaranje (alt+f4) prozora zatvara trenutno aktivan prozor,
-- to jest, brise glavu liste
closeWindow :: Ring t -> Ring t
closeWindow ([], [])   = ([], [])
closeWindow ([w], rs)  = (reverse rs, [])
closeWindow (l:ls, rs) = (ls, rs)


-- Kad se prozor otvori, postaje aktivan i dolazi na vrh liste
openWindow :: t -> Ring t -> Ring t
openWindow w (ls, rs) = (w:ls, rs)



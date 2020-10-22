module Lib where

-- [...] Nastavak
--
-- Posto u prethodnom primeru koristimo alias za tip,
-- korisnik moze da prosledjuje obicne liste nasim funkcijama
--
-- Da bismo to sprecili, mozemo koristiti `newtype` deklaraciju
-- tipa. Tipovi ovako deklarisani moraju imati samo jedno polje
-- i Haskell nece dozvoliti implicitnu konverziju iz liste u Ring
-- Posto ovo nije alias za tip, definisemo konstruktor `MkRing` 
-- putem kojeg cemo kreirati objekat tipa Ring
newtype Ring t = MkRing [t]


-- Sada moramo izmeniti implementaciju tako da se svuda koristi
-- konstruktor `MkRing` - povratna vrednost vise ne moze
-- biti lista nego Ring

fromList :: [t] -> Ring t
fromList = MkRing

toList :: Ring t -> [t]
toList (MkRing xs) = xs

-- ime@pattern se moze koristiti ako zelimo da zadrzimo ime objekta koji
-- zelimo da dekonstruisemo  
switchActive :: Ring t -> Ring t
switchActive r@(MkRing [])                 = r 
switchActive r@(MkRing [w])                = r
switchActive (MkRing (active:next:others)) = (MkRing (next:active:others))

focusNext :: Ring t -> Ring t
focusNext r@(MkRing [])   = r
focusNext (MkRing (x:xs)) = MkRing (xs ++ [x]) 

-- ovde sad ne mozemo iskoristiti kompoziciju funkcija
-- posto focusNext radi nad Ring-om
--
-- `undefined` se moze koristiti kao "placeholder" za definicije funkcija
-- poziv funkcije cija je definicija `undefined` uzrokuje izuzetak
focusPrev :: Ring t -> Ring t
focusPrev (MkRing xs) = undefined    -- implementirati

-- Nekad je korisno videti sta Haskell dedukuje kao tip odredjenog izraza
-- U tim slucajevima mozemo koristiti "rupe" (holes) - tipske promenljive 
-- koje pocinju donjom crtom. Prevodilac ce dati detaljan opis koji ukljucuje
-- tip izraza kao i "okruzenje" - tipovi simbola koji su vidljivi u kontekstu
-- u kom se nalazi rupa
-- Primer koriscenja rupe:
--
-- focusPrev (MkRing xs) = _a

-- Rupe se mogu koristiti i u okviru slozenijih izraza, sto ce biti 
-- demonstrirano kasnije tokom kursa

closeWindow :: Ring t -> Ring t
closeWindow (MkRing xs) = MkRing (tail xs)

openWindow :: t -> Ring t -> Ring t
openWindow x (MkRing xs) = MkRing (x:xs)



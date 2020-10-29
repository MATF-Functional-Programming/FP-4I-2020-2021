module Lib where

data UList a = Empty
             | Cons a (UList a)
             deriving Show

-- Tri slucaja tek radi demonstracije
ulConcat :: UList a -> UList a -> UList a
ulConcat Empty rs          = rs
ulConcat (Cons l Empty) rs = Cons l rs
ulConcat (Cons l ls) rs    = Cons l (ulConcat ls rs)


infixr 7 :/:
data OList a = End
             | (:/:) a (OList a)

infixr 6 +/+
(+/+) :: OList a -> OList a -> OList a
End         +/+ rs = rs
(l :/: End) +/+ rs = l :/: rs
(l :/: ls)  +/+ rs = l :/: (ls +/+ rs)


-- Da bismo prikazali listu elemenata tipa `a`,
-- neophodno je da znamo kako da prikazemo jedan element
-- tog tipa -- tip `a` mora isto da implementira `Show`
instance Show a => Show (OList a) where
    show End = ""
    show (x :/: End) = show x
    show (x :/: xs)  = show x ++ ", " ++ show xs


-- Funkcija `fmap` transformise elemente kolekcije
-- i kao rezultat vraca kolekciju transformisanih elemenata
instance Functor OList where
    fmap f End = End
    fmap f (x :/: xs) = f x :/: fmap f xs


-- Funkcija `foldr` prolazi kroz kolekciju (sa desne strane)
-- i akumulira sve elemente u kolekciji
instance Foldable OList where
    foldl f acc End        = acc
    foldl f acc (x :/: xs) =
            let newAcc = f acc x
            in  foldr f newAcc xs

    foldr f init End        = init
    foldr f init (x :/: xs) =
            f x (foldr f init xs)




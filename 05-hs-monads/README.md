# Alternative, Monad

### Alternatives
- Alternativi se nalaze u `Control.Applicative`, kao i aplikativi. Pogledajmo definiciju klase:
    ```hs
    class Applicative f => Alternative (f :: * -> *) where
        empty :: f a
        (<|>) :: f a -> f a -> f a
        some :: f a -> f [a]
        many :: f a -> f [a]
    ```

- Vidimo da je neophodno da je tip aplikativ da bi mogao biti i alternativ
- Potrebno je implementirati `empty` i `<|>`. Pogledajmo na primeru kako alternativi funkcionisu (`Maybe` je alternativ):
    ```hs
    ghci> Just 3 <|> Just 1 <|> Nothing
    Just 3

    ghci> Nothing <|> Just 1 <|> Nothing
    Just 1
    ```

- `empty` je identitet za `<|>` - nula rezultata
- Operator `<|>` - spajanje svih mogucih rezultata iz vise izracunavanje u jedno
- Ako posmatramo `Maybe` vrednost kao rezultat nekog izracunavanja koje moze da uspe (`Just x`) i da ne uspe (`Nothing`), operator `<|>` treba da spoji dva takva izracunavanja u jedno, ali posto `Maybe` moze da sadrzi samo jedan rezultat, uzima se prvi rezultat koji postoji (odnosno prvi koji nije `Nothing`) 
- Slicno, i liste mozemo da posmatramo kao izracunavanja koja mogu da imaju nula ili vise rezultata
- U ovom slucaju, kada hocemo da iskombinujemo dva izracunavanja u jedno, mozemo da sacuvamo sve moguce rezultate - samo nadovezemo dve liste
- Kako instancirati `Applicative` za `Maybe`?
    ```hs
    instance Alternative Maybe where
        empty = Nothing

        Nothing <|> r = r
        l       <|> _ = l
    ```
- Alternative pominjemo ovde jer cemo ih koristiti na narednim casovima. U kontekstu hijerarhije `Functor a => Applicative a => Monad a`, oni su zasebna klasa.

### Monads
- Ako imamo vrednost sa kontekstom `m a`, kako da ga damo funkciji `a -> m b`?
- Ukoliko resimo ovaj problem, mozemo praviti i kompozicije ovakvih funkcija!
- Zelimo funkciju koja radi: `(Monad m) => m a -> (a -> m b) -> m b` 
- _Bind_ funkcija `:t (>>=)`
- **Monade** su aplikativni funktori koji definisu i bind (`>>=`)
- `:t (>>=)`
- `[]`, `Maybe`, `Either`, `[]`, `(->) r`, `IO` su monade
- Kako bismo definisali `>>=` za `Maybe`?
    ```hs
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b  
    Nothing  >>= f = Nothing  
    (Just x) >>= f = f x  
    ```
- Primeri
    ```hs 
    gchi> let f = \x -> Just (x+1)
    ghci> f 1  
    Just 2
    ghci> Just 3 >>= f 
    Just 4  
    ghci> Nothing >>= f 
    Nothing  
    ghci> Just "smile" >>= \x -> Just (x ++ " :)")  
    Just "smile :)"  
    ghci> Nothing >>= \x -> Just (x ++ " :)")  
    Nothing  
    ghci> Just 3 >>= \x -> if x > 2 then Just x else Nothing  
    Just 3  
    ghci> Just 1 >>= \x -> if x > 2 then Just x else Nothing  
    Nothing  
    ```
- `:info Monad`
    - razlika `>>` i `>>=`, kada koristiti koji?
    - `>>` dolazi sa podrazumevanom implementacijom
        ```hs
        ghci> Nothing >> Just 3  
        Nothing  
        ghci> Just 3 >> Just 4  
        Just 4  
        ghci> Just 3 >> Nothing  
        Nothing  
        ```
    - `return` isto sto i `pure`
    - `fail` se ne koristi u nasim kodovima, vec se koristi unutar jezika
- Kako bismo "dokazali" da je `Maybe` instanca `Monad` klase?
    ```hs
    instance Monad Maybe where  
        return x = Just x  
        Nothing >>= f = Nothing  
        Just x >>= f  = f x  
        fail _ = Nothing  
    ```
- Videti _Pierre_ primer
- Kako bismo "dokazali" da je `[]` instanca `Monad` klase?
    ```hs
    instance Monad [] where  
        return x = [x]  
        xs >>= f = concat (map f xs)  
        fail _ = []  

    ghci> [3,4,5] >>= \x -> [x,-x]  
    [3,-3,4,-4,5,-5]  
    ```
- Zakoni:
    - _levi identitet_ : `return x >>= f`  je isto sto i `f x` (_levi identitet_)
    - _desni identitet_: `m >> return`     je isto sto i `m`   (_desni identitet_)
    - _asocijativnost_ : `(m >>= f) >>= g` je isto sto i `m >>= (\x -> f x >>= g)` 
- Videti primer sa vezbi
- Postoji specijalna notacija u sintaksi Haskell-a za monade zvana `do` notacija
    ```hs
    foo :: Maybe String  
    foo = Just 3   >>= (\x -> 
          Just "!" >>= (\y -> 
          Just (show x ++ y)))  
    
    foo :: Maybe String  
    foo = do  
        x <- Just 3  
        y <- Just "!"  
        Just (show x ++ y) 
    ```

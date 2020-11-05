# Functor, Applicative

### Functor
- `Functor` - sve sto implementira `fmap` ili `<$>` (infiksni `fmap`)
- `kind`, za `Functor`, je `* -> *` (videti: `:info Functor`) 
- Stoga `instance Functor Either` ne moze, vec `instance Functor (Either a)` - mora biti tacno **1** slobodan tipski parametar
- Neki funktori: `Maybe a`, `Either a`, `[]`, `IO` (svi su oni vise od funktora ali o tome kasnije)
- Videti implementaciju u [izvornom fajlu](02_applicative/src/Main.hs).
- `fmap` se u karijevskom stilu vidi kao: `fmap :: (a -> b) -> (f a -> f b)` 
- Ovo se naziva _lifting_ - podizemo funkciju jednog argumenta da radi nad funktorima
    ```hs
    ghci> :t fmap (*2)  
    fmap (*2) :: (Num a, Functor f) => f a -> f a  
    
    ghci> :t fmap (replicate 3)  
    fmap (replicate 3) :: (Functor f) => f a -> f [a]  
    
    ghci> fmap (replicate 3) [1,2,3,4]  
    [[1,1,1],[2,2,2],[3,3,3],[4,4,4]]  
    
    ghci> fmap (replicate 3) (Just 4)  
    Just [4,4,4]  
    
    ghci> fmap (replicate 3) (Right "blah")  
    Right ["blah","blah","blah"]  
    
    ghci> fmap (replicate 3) Nothing  
    Nothing  
    
    ghci> fmap (replicate 3) (Left "foo")  
    Left "foo"  
    ```
- Zakoni funktora:
    - `fmap id = id`
    - `fmap (f . g) = fmap f . fmap g`
- `(->) r` je takodje funktor. Ali sta je to?
    `r -> a` se moze pisati prefiksno kao `(->) r a` - funkcija 2 argumenta, stoga se mora primeniti delimicno zbog 
    kind-a Functor klase.
    U `Control.Monad.Instances` se nalazi implementacija

    ```hs
    instance Functor ((->) r) where
        fmap f g = (\x -> f (g x))
    ```
        
    - Tumacenje:
        - Krecemo od potpisa za fmap: `fmap :: (a -> b) -> f a -> f b`
        - Zameniti `f` sa `((->) r)`: `fmap :: (a -> b) -> ((->) r a) -> ((->) r b)`
        - Napisemo infiksno: `fmap :: (a -> b) -> (r -> a) -> (r -> b)`
        - Podseca na nesto? Kompozicija funkcija!

    ```hs
    instance Functor ((->) r) where
        fmap f g = (.)
    ```   
    
- Primeri: 
    ```hs
    ghci> :t fmap (*3) (+100)  
    fmap (*3) (+100) :: (Num a) => a -> a  
    
    ghci> fmap (*3) (+100) 1  
    303  
    
    ghci> (*3) `fmap` (+100) $ 1  
    303  
    
    ghci> (*3) . (+100) $ 1  
    303  
    
    ghci> fmap (show . (*3)) (*100) 1  
    "300"  
    ```

### Applicative
 
- Sta ako zelimo da mapiramo nad funkcijama 2 ili vise argumenata, npr `(*)` ili `(++)`?
    ```hs
    ghci> :t fmap (++) (Just "hey")  
    fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])  
    ```
- Sta ako imamo `Just (3*)` i `Just 5` i zelimo da uzmemo `(3*)` i mapiramo nad `Just 5`
- Nemoguce po logici funktora, zato koristimo aplikative (`Control.Applicative`)
    ```hs
    class (Functor f) => Applicative f where  
        pure :: a -> f a  
        (<*>) :: f (a -> b) -> f a -> f b  
    ```
    - Objasnjenje:
        - pure  - ubacuje u kutiju - kreira kontekst/funktor od vrednosti
        - `<*>` - videti `:t (<*>)`, pojacana verzija `fmap`, prvo izvuce funkciju iz funktora a zatim je primeni

- Primeri:
    ```hs    
    ghci> Just (+3) <*> Just 9  
    Just 12  
    
    ghci> pure (+3) <*> Just 10  
    Just 13  
    
    ghci> pure (+3) <*> Just 9  
    Just 12  
    
    ghci> Just (++"hahah") <*> Nothing  
    Nothing  
    
    ghci> Nothing <*> Just "woot"  
    Nothing  
    ```

- Mozemo koristeci aplikative da mapiramo funkciju vise argumenata preko funktora
    ```hs  
    ghci> pure (+) <*> Just 3 <*> Just 5  
    Just 8  
    
    ghci> pure (+) <*> Just 3 <*> Nothing  
    Nothing  

    ghci> pure (+) <*> Nothing <*> Just 5  
    Nothing  
    ```

- `pure f <*> x` je isto sto i `fmap f x`, stoga:
    ```hs
    ghci> (++) <$> Just "johntra" <*> Just "volta"  
    Just "johntravolta"  
    ```

- `Maybe a` je `Applicative`:
    ```hs
    instance Applicative Maybe where
        pure x = Just x

        (Just f) <*> (Just x) = Just (f x)
              _  <*>  _       = Nothing
    ```

- `[]` je `Applicative`
    ```hs
    instance Applicative [] where  
        pure x = [x]  
        fs <*> xs = [f x | f <- fs, x <- xs]  
    ```

    Primeri:
    ```hs
    ghci> [(*0),(+100),(^2)] <*> [1,2,3]  
    [0,0,0,101,102,103,1,4,9]  
    
    ghci> [(+),(*)] <*> [1,2] <*> [3,4]  
    [4,5,5,6,3,4,6,8] 
    ```
    
    Ne moze tip da ima 2 implementacije `pure` i `<*>` pa stoga postoji i `ZipList` koja radi 
    aplikaciju prve funkcije prve liste sa prvom vrednoscu druge liste itd.
    
- `liftA2` iz `Control.Applicative` radi lift binarne funkcije
    `liftA2 f a b = f <$> a <*> b`

    ```hs
    ghci> liftA2 (:) (Just 3) (Just [4]) 
    Just [3,4] 

    ghci> (:) <$> Just 3 <*> Just [4]  
    Just [3,4]  
    ```

- Ako imamo listu aplikativa, da li je moguce kombinovati proizvoljan broj aplikativa u jedan tako da on sadrzi listu vrednosti svih tih aplikativa?
    ```hs
    sequenceA :: (Applicative f) => [f a] -> f [a]  
    sequenceA [] = pure []  
    sequenceA (x:xs) = (:) <$> x <*> sequenceA xs  

    sequenceA :: (Applicative f) => [f a] -> f [a]  
    sequenceA = foldr (liftA2 (:)) (pure [])  
    ```

    ```hs
    ghci> sequenceA [Just 3, Just 2, Just 1]  
    Just [3,2,1]
  
    ghci> sequenceA [Just 3, Nothing, Just 1]  
    Nothing  

    ghci> sequenceA [(+3),(+2),(+1)] 3  
    [6,5,4]  

    ghci> sequenceA [[1,2,3],[4,5,6]]  
    [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]  

    ghci> sequenceA [[1,2,3],[4,5,6],[3,4,4],[]]  
    []

    ghci> sequenceA [getLine, getLine, getLine] 
    heyh  
    ho  
    woo  
    ["heyh","ho","woo"]  
    ```






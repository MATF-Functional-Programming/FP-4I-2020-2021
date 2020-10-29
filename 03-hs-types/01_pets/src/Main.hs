module Main where

main :: IO ()
main = do
    putStrLn "hello world"


-- Ova sintaksa generise konstruktor MkPet,
-- i nesvrstane funkcije (getere) za svako polje strukture
data Pet = MkPet { regId  :: Int
                 , name   :: String
                 , age    :: Int
                 , height :: Float
                 , weight :: Float
                 } deriving (Show)


-- Mozemo da instanciramo tip na 'stari' nacin - koji smo imali
-- sa obicnim tipovima koje smo definisali preko `data` kljucne reci
yourPet = MkPet 1 "Pera" 5 10.1 50.2

-- A mozemo i eksplicitno da navedemo polja koja inicijalizujemo
myPet = MkPet { regId = 1, name = "Pera", age = 5, height = 10.1, weight = 50.2 }

-- Ako zelimo da napravimo kopiju vec postojeceg objekta, samo
-- sa promenjenim nekim poljem, mozemo da iskoristimo predjasnju
-- sintaksu ovako:
myPetOlder = myPet { age = 6 }



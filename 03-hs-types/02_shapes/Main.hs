data Circle = MkCircle { r :: Float }
data Rectangle = MkRectangle { a :: Float, b :: Float }

class Shape a where
    area          :: a -> Float
    circumference :: a -> Float


instance Shape Circle where
    area          x = r' * r' * pi where r' = r x
    circumference x = 2 * r' * pi where r' = r x

instance Eq Circle where
    c1 == c2 = (r c1) == (r c2)

instance Show Circle where
    show x = "(r=" ++ show (r x) ++ ")"

    
instance Shape Rectangle where
    area          x = (a x) * (b x)
    circumference x = 2 * ((a x) + (b x))

instance Eq Rectangle where
    r1 == r2 = (a r1) == (a r2) && (b r1) == (b r2)
    
instance Show Rectangle where
    show r = "(a=" ++ show (a r) ++ ",b=" ++ show (b r) ++ ")";

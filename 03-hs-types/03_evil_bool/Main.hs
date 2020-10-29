
class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _  = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno _       = True

-- yesno 1
-- yesno ""
-- yesno $ Just 1

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf val yesResult noResult = if yesno val then yesResult else noResult



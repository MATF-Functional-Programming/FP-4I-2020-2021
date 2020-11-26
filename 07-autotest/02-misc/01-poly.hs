instance Num a => Num [a] where
    -- (+)
    (x:xs) + (y:ys) = x+y : xs+ys
    xs + [] = xs
    [] + ys = ys

    -- (*)
    (x:xs) * (y:ys) = x*y : [x]*ys + xs*(y:ys)
    _ * _ = []

    -- other functions
    abs           = undefined           -- TODO
    signum        = map signum
    fromInteger n = [fromInteger n]
    negate        = map (\x -> -x)


-- infinite Pascal triangle
pascal = map ([1,1]^) [0..]

-- take 5 pascal : [[1], [1,1], [1,2,1], [1,3,3,1], [1,4,6,4,1]]

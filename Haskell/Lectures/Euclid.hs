extEuclid :: Integral a => a -> a -> a -> (a, a)
extEuclid a b c    
    | (b == 0) && (c `mod` a /= 0)  = error
        "the equation has no solution"
    | b == 0 = (c `div` a, 0)
    | otherwise = (x, (c - a*x) `div` b)
    where a'' = a `mod` b
          c'' = c `mod` b         
          (x', y') = extEuclid b a'' c''
          x = y'

main = do    
    print (extEuclid 715 273 91)
    let (x, y) = extEuclid 715 273 91
    print (715*x+273*y)
    
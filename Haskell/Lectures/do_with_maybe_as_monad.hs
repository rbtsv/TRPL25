safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

calc :: Double -> Double -> Double -> Maybe Double
calc a b c = do
    x <- safeDiv a b -- a / b
    y <- safeDiv x c -- (a/ b) / c
    return y
    
calc' :: Double -> Double -> Double -> Maybe Double
calc' a b c =
    safeDiv a b >>= (\x -> safeDiv x c) >>= (\y -> return y)    


main :: IO ()
main = do
    print $ calc 10 2 5    -- Just 1.0
    print $ calc 10 0 5    -- Nothing
    print $ calc' 10 2 0   -- Nothing

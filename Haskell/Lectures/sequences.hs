nat = 0:map (+1) nat
nat1 = 1:zipWith (+) nat1 (repeat 1)
fibs = 1:1:zipWith (+) fibs (tail fibs)


main = do    
    print (take 100 nat)
    print (take 100 nat1)
    print (take 100 fibs)
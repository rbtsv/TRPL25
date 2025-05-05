f' x y z = x + y + z 
f'' = \x y z -> x + y + z 
f = \x -> (\y -> (\z -> x + y + z))
g = f 2
h x z = f' x 0 z 

main = do
    print (f 1 1 1 )    
    print (g 1 1  )
    print (h 1 1  )



list = [3,2,1,7,5,6,4,2,1,5,9,8,8]

tst [] = []
tst [x] = [x]
tst [x,y]
    | (x==y) = [x]
    | otherwise = [x,y]
tst xs = xs

main = do
    print $ tst [1,1]
    --print (list)

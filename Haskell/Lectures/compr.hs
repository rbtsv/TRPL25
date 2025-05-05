l = [1..10]
list = [3,2,1,7,5,6,4,2,1,5,9,8,8]

ll = [x^2 | x <- l, x `mod` 2 == 0]
fl = map (^2) $ filter (\x -> (x `mod` 2) == 0) l

main = do
    print l --xs
    print ll
    print fl
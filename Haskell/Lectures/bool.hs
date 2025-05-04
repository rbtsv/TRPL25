data BOIL = O | I deriving Show

data TBOIL = Node TBOIL TBOIL | T BOIL deriving Show

t = Node (T O) (T I)
ft = Node (Node (T I) (T O)) (T O)

l = [[O, I], [I, I]]

{- TRUTH TABLE
[O, O, ],
[O, I, ],
[I, O, ],
[I, I, ]
-}

xor' = [[O, O, O],
        [O, I, I],
        [I, O, I],
        [I, I, O]]

comp (T y) _ = y
comp (Node n0 n1) [] = comp n0 []
comp (Node n0 n1) (O:xs) = comp n0 xs
comp (Node n0 n1) (I:xs) = comp n1 xs

f = comp ft

dtree [] = T O
dtree [[y]] = T y
dtree xs = Node (dtree [os | O:os <- xs]) (dtree [is | I:is <- xs])

l_xor = comp $ dtree xor'

xor x y = l_xor [x, y]



main = do      
    print l
    --print $ part l
    print $ dtree l
    print $ l_xor [O, O]
    print $ l_xor [O, I]
    print $ l_xor [I, O]
    print $ l_xor [I, I]
    print $ O `xor` I `xor` I `xor` O `xor` I
    {-print l
    print t
    print $ comp t [O]
    print $ comp t [I]
    print $ comp ft []
    print $ comp ft [O]
    print $ f [I]
    print $ f [O,I]
    print $ f [O,I, O]-}

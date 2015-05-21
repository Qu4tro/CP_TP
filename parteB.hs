type Point  = (Int, Int)
type Side   = Int
type Tri    = (Point, Side)

data TLTree = Tri Tri | Nodo TLTree TLTree TLTree

sierpinski :: Tri -> Int -> [Tri]
sierpinski t = apresentaSierp . (geraSierp t)

geraSierp :: Tri -> Int -> TLTree
geraSierp t         0 = Tri t      
geraSierp ((x,y),s) n =
     let s' = div s 2
     in  Nodo
           (geraSierp ((x,y), s') (n-1))
           (geraSierp ((x+s',y), s') (n-1))
           (geraSierp ((x,y+s'), s') (n-1))


apresentaSierp :: TLTree -> [Tri]
apresentaSierp (Tri t      ) = [t]
apresentaSierp (Nodo a b c)  = (apresentaSierp a)++(apresentaSierp b)++(apresentaSierp c)  

ts = geraSierp tri 5 where tri = ((0, 0), 256)

test06a = depthTLTree ts == 6
test06b = countTLTree ts == 243
test06c = countTLTree ts == length (tipsTLTree ts)
test06d = countTLTree ts == countTLTree (invTLTree ts)

inTLTree :: Either Tri (TLTree, TLTree, TLTree) -> TLTree
inTLTree = either Tri (Nodo, Nodo, Nodo)

outTLTree :: TLTree -> Either Tri (TLTree, TLTree, TLTree)
outTLTree (Tri tri)       = i1 tri
outTLTree (Nodo t1 t2 t3) = i2 (t1, t2, t3)

-- baseTLTree :: (a -> b) -> (c -> d) -> Either e (a, (c, c)) -> Either e (b, (d, d))
baseTLTree = 

-- recTLTree :: (a -> b) -> Either c (d, (a, a)) -> Either c (b, b )
recTLTree f = baseTLTree id f

cataTLTree :: 
cataTLTree = undefined

anaTLTree :: 
anaTLTree f = undefined

hyloTLTree :: 
hyloTLTree a c = undefined

tipsTLTree :: 
tipsTLTree = undefined

invTLTree :: 
invTLTree = undefined

depthTLTree :: 
depthTLTree = undefined

geraSierp :: Tri -> Int -> TLTree Tri
geraSierp = undefined

countTLTree :: TLTree b -> Int
countTLTree = undefined

draw = render html 
    where html = rep dados

rep = undefined

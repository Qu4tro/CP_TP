import Cp
import LTree
import BTree

balanced :: LTree a -> Bool
balanced (Leaf _) = True
balanced (Fork (t, t')) = balanced t && balanced t' && abs(depth t - depth t') <= 1

depth :: LTree a -> Integer
depth = cataLTree (either one (succ . uncurry max))

balance :: LTree a -> LTree a
balance = undefined

t = Fork (Fork (Leaf 10, 
                Fork (Leaf 2,
                      Fork (Leaf 5, 
                            Leaf 3))),
          Leaf 23)

test01 = balanced t == False
test02 = balanced (balance t) == True


abpe :: (Int, Int) -> BTree Int
abpe (n,m) = anaBTree qsplit (n,m)

qsplit :: Integral a => (a, a) -> Either () (a, ((a, a), (a, a)))
qsplit (n, m) 
    | n >  m     = i1 ()
    | m == 0     = i1 ()
    | m == n     = i2 (m, ((m, m - 1), (m + 1, m)))
    | otherwise  = i2 (half (n, m), ((n, half (n, m) - 1), (half (n, m) + 1, m)))

half :: Integral a => (a, a) -> a
half (n, m) = (div (m - n) 2) + n

t1 = abpe(20,30)

test03a = qsplit (4,30) == i2(17,((4,16),(18,30)))
test03b = qsplit (4,3) == i1()
test03c = qsplit (0,0) == i1()
test03d = qsplit (1,1) == Right (1,((1,0),(2,1)))
test03e = balBTree t1 == True
test03f = inordt t1 == [20..30]
test03 = test03a &&
         test03b && 
         test03c && 
         test03d && 
         test03e && 
         test03f

data SList a b = Sent b | Cons (a,SList a b) deriving (Show,Eq)

inSList :: Either a (a1, SList a1 a) -> SList a1 a
inSList = either Sent Cons

outSList :: SList a b -> Either b (a, SList a b)
outSList (Sent b) = i1 b
outSList (Cons (a, sl)) = i2 (a, sl)

recSList :: (a -> b) -> Either c (d, a) -> Either c (d, b)
recSList f = id -|- (id >< f)

cataSList :: (Either b (a, d) -> d) -> SList a b -> d
cataSList g = g . (recSList (cataSList g)) . outSList

anaSList :: (c -> Either a (b, c)) -> c -> SList b a
anaSList g = inSList . (recSList (anaSList g)) . g

hyloSList :: (Either b (d, c) -> c) -> (a -> Either b (d, a)) -> a -> c
hyloSList f g = cataSList f . anaSList g

mgen :: Ord a => ([a], [a]) -> Either [a] (a, ([a], [a]))
mgen = undefined

test04a = let x = Cons(1,Sent "end") in inSList(outSList x) == x
test04b = let x = i2("ola",Sent "2") in outSList(inSList x) == x
test04 = test04a && test04b

merge' :: Ord a => ([a], [a]) -> [a]
merge' = hyloSList (either id cons) mgen

test05a = mgen      ([0,2,5],[0,6]) == i2 (0,([2,5],[0,6]))
test05b = mgen ([0, 2, 5], []) == i1 [0,2,5]
test05c = merge' ([],[0,6]) == [0,6]
test05 = test05a && test05b && test05c

tests = test01 &&
        test02 &&
        test03 &&
        test04 &&
        test05

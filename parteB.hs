import Cp
import X3d
import Data.List
-- import System.Process

x3dom :: (Foldable t) => t String -> String
x3dom = html . preamble . body . x3d . scene . items
-- x3dom = undefined

html = tag "html" []

preamble rest = (headx $ concat [title "CP/X3DOM generation",links,script]) ++ rest

-- preamble = undefined

body = tag "body" []

x3d = tag "x3d" [("width","\"500px\""),("height","\"400px\"")]

scene = tag "scene" []

items :: (Foldable t) => t [a] -> [a]
items = concat

links = ctag "link" [
    ("rel",quote "stylesheet"),("type",quote "text/css"),
    ("href",quote "http://www.x3dom.org/x3dom/release/x3dom.css")]

script = ctag "script" [
    ("type",quote "text/javascript"),
    ("src",quote "http://www.x3dom.org/x3dom/release/x3dom.js")]

ctag t l = tag t l ""
tag t l x = "<"++t++" "++ps++">"++x++"</"++t++">"
     where ps = unwords [concat[t,"=",v]| (t,v)<-l]

headx = tag "head" []

title = tag "title" []

quote s = "\""++s++"\""


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
test06c = countTLTree ts == genericLength (tipsTLTree ts)
test06d = countTLTree ts == countTLTree (invTLTree ts)
test06 = test06a && test06b && test06c && test06d

inTLTree :: Either Tri (TLTree, (TLTree, TLTree)) -> TLTree
inTLTree = either Tri joinTrees

joinTrees :: (TLTree, (TLTree, TLTree)) -> TLTree
joinTrees (t1, (t2, t3)) = Nodo t1 t2 t3

outTLTree :: TLTree -> Either Tri (TLTree, (TLTree, TLTree))
outTLTree (Tri tri)       = i1 tri
outTLTree (Nodo t1 t2 t3) = i2 (t1, (t2, t3))

baseTLTree g f = (g >< g) -|- (f >< (f >< f))

recTLTree :: (a -> b) -> Either (c, d) (a, (a, a)) -> Either (c, d) (b, (b, b))
recTLTree f = (id >< id) -|- (f >< (f >< f))

cataTLTree :: (Either Tri (a, (a, a)) -> a) -> TLTree -> a
cataTLTree g = g . (recTLTree (cataTLTree g)) . outTLTree

anaTLTree :: (a -> Either Tri (a, (a, a))) -> a -> TLTree
anaTLTree f = inTLTree . (recTLTree (anaTLTree f)) . f

hyloTLTree a c = cataTLTree a . anaTLTree c

--tipsTLTree == apresentaSierp
tipsTLTree :: TLTree -> [Tri]
tipsTLTree = cataTLTree (either singl concatenate)
    where concatenate (a, (b, c)) = a ++ b ++ c

invTLTree :: TLTree -> TLTree
invTLTree = cataTLTree (either Tri invTuple)
    where invTuple (t1, (t2, t3)) = Nodo t3 t2 t1

-- invTLTree :: TLTree -> TLTree
-- invTLTree (Tri tri)       = Tri tri
-- invTLTree (Nodo t1 t2 t3) = Nodo (invTLTree t3) (invTLTree t2) (invTLTree t1)

depthTLTree :: TLTree -> Integer
depthTLTree = cataTLTree (either one (succ . max3)) 
    where max3 (a, (b, c)) = max a (max b c)

countTLTree :: TLTree -> Integer
countTLTree = cataTLTree (either one add3)
    where add3 (a, (b, c)) = a + b + c


draw = render html 
    where html = rep dados

render :: String -> IO ()
render html = writeFile "_.html" html

dados :: (Tri, Int)
dados = (((0, 0), 32), 4)

rep dados = x3dom (map drawTriangle (tipsTLTree (uncurry geraSierp dados)) ) 

main = draw

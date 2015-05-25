import Cp
import BTree
import Probability

import Control.Parallel.Strategies

pcataList :: (Either () (a, b) -> Dist b) -> [a] -> Dist b
pcataList g = mfoldr (curry (g.i2)) ((g.i1) ()) where
    mfoldr f d [] = d
    mfoldr f d (a:x) = do { y <- mfoldr f d x ; f a y }

gene :: Either () (String, [String]) -> Dist [String]
gene = (either empty transmitter)

transmitter :: (String, [String]) -> Dist [String]
transmitter ("stop", words) = D [("stop":words, 0.90), (words, 0.10)]
transmitter (word,   words) = D [(  word:words, 0.95), (words, 0.05)]

empty :: Monad m => () -> m [t]
empty () = return []

transmitir = (pcataList gene) . (++ ["stop"])

test07 = gene (i2("a",["b"])) == D [(["a","b"],0.95),(["b"],0.05)]

ataque = transmitir (words "Vamos atacar hoje")

parmap :: (a -> b) -> [a] -> Eval [b]
parmap f [] = return []
parmap f (a:lt) = do
    a' <- rpar (f a)
    lt' <- parmap f lt
    return (a':lt')

parBTreeMap :: (a -> b) -> (BTree a) -> Eval (BTree b)
parBTreeMap f Empty                  = return Empty
parBTreeMap f (Node (a, (bt1, bt2))) = do a'   <- rpar (f a)
                                          bt1' <- parBTreeMap f bt1
                                          bt2' <- parBTreeMap f bt2
                                          return (Node (a', (bt1', bt2')))

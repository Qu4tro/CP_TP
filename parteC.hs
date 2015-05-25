import Cp
import Probability

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



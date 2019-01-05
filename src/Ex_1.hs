module Ex_1 where

import Preamble

-- Use Curry–Howard to prove the exponent law that a ^ b × a ^ c = a ^ (b + c)
toI :: (b -> a) -> (c -> a) -> Either b c -> a
toI f _ (Left b)  = f b
toI _ g (Right c) = g c

fromI :: (Either b c -> a) -> (b -> a, c -> a)
fromI f = (f . Left, f . Right)


-- Prove (a × b) ^ c = a ^ c × b ^ c
toII :: (c -> (a, b)) -> (c -> a, c -> b)
toII f = (fst . f, snd . f)

fromII :: (c -> a, c -> b) -> c -> (a, b)
fromII (ca, cb) c = (ca c, cb c)


-- (a ^ b) ^ c = a ^ (b × c)
toIII :: (c -> b -> a) -> (b, c) -> a -- reminds uncurry
toIII f (b, c) = f c b

fromIII :: ((b, c) -> a) -> c -> b -> a -- reminds curry
fromIII f c b = f (b, c)

// http://codereview.stackexchange.com/questions/104322/a-better-fizzbuzz

import Data.List (unfoldr)

toFizzBuzz :: Int -> Int -> [String]
toFizzBuzz start count = 
    let fizz = const "fizz"
        buzz = const "buzz"
        fizzbuzz = const "fizzbuzz"
        fizzbuzzFuncs =  cycle [show, show, fizz, show, buzz, fizz, show, show, fizz, buzz, show, fizz, show, show, fizzbuzz]
        offsetFuncs = drop (mod (start - 1) 15) fizzbuzzFuncs
        step (i, fs) = Just((head fs) i, (i + 1, tail fs))
    -- in take count $ zipWith ($) offsetFuncs [start..]
    in take count $ unfoldr step (start, offsetFuncs)

--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab 3: Recursive and higher-order functions                                --
--------------------------------------------------------------------------------

module Lab3 where

--------------------------------------------------------------------------------

-- Some of the functions we will be defining as part of this lab are
-- part of Haskell's standard library. The following line tells the compiler
-- not to import them.
import Prelude hiding ( Monoid(..), elem, maximum, intersperse, transpose
                      , subsequences, permutations, any, all, flip, takeWhile
                      , zipWith, groupBy, notElem )

--------------------------------------------------------------------------------
-- Recursive and higher-order functions

elem :: Eq a => a -> [a] -> Bool
-- elem n = foldr (\v b -> v == n || b) False
elem n x = not (null (filter (\y -> y == n) x))

maximum :: Ord a => [a] -> a
-- maximum (x:xs) = case length xs of
--     0 -> x
--     _ -> if maximum xs > x then maximum xs else x
maximum = foldr1 (\x y -> if x > y then x else y)

intersperse :: a -> [a] -> [a]
intersperse sep (x:xs) = case length xs of
    0 -> [x]
    _ -> [x] ++ [sep] ++ intersperse sep xs
intersperse sep emptyString = emptyString

any :: (a -> Bool) -> [a] -> Bool
any fn list = not (null (filter fn list))
-- use any where fn = \x -> x == n for elem using any

all :: (a -> Bool) -> [a] -> Bool
all fn list = not (any (\val -> not (fn val)) list)

flip :: (a -> b -> c) -> b -> a -> c
flip fn x y = fn y x

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile fn [] = []
takeWhile fn (x:xs) =
    if fn x then
        x : takeWhile fn xs
    else
        []

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith fn (x:xs) (y:ys) = fn x y : zipWith fn xs ys
zipWith fn _ _ = []

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy fn [] = []
groupBy fn (x:xs) = case length xs of
    0 -> [[x]]
    _ -> let groups = groupBy fn xs
        in 
            if fn ((groups !! 0) !! 0) x then
                let (y:ys) = groups
                in (x : y) : ys
            else    
                [x] : groups




subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = subsequences xs ++ [x:ys | ys <- subsequences xs]

permutations :: Eq a => [a] -> [[a]]
permutations = undefined
-- permutations [] = []
-- permutations [x] = [[x]]
-- permutations (x:xs) = foldr (\val acc -> acc ++ perms x val) [] (permutations xs)
--     where perms x p = [take n p ++ [x] ++ drop n p | n <- [0..(length p)]]

--------------------------------------------------------------------------------
-- Monoids

-- Monoid laws:
--
-- (Left identity)      mappend mempty x = x
-- (Right identity)     mappend x mempty = x
-- (Associativity)      mappend x (mappend y z) = mappend (mappend x y) z
-- (mconcat)            mconcat = foldr mappend mempty

class Monoid a where
    mempty  :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a
    mconcat = foldr mappend mempty

instance Monoid Int where
    mempty  = 0
    mappend = (+)

instance Monoid [a] where
    mempty  = []
    mappend = (++)

instance Monoid b => Monoid (a -> b) where
    mempty  = mempty
    mappend = undefined

--------------------------------------------------------------------------------

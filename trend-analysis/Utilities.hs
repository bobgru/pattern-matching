-- |Utility functions that have not been grouped into coherent modules.
module Utilities
where

import Data.List(sortBy)

-- |Given a list, pair each element with its index so it can be
-- sorted later.
zipWithIndex :: [a] -> [(a,Int)]
zipWithIndex xs = zip xs [1..]

-- |Sort by index and strip.
sortStripList :: [(a,Int)] -> [a]
sortStripList = map fst . sortBy cmp
    where
        cmp a b = compare (snd a) (snd b)

-- |Augment each element of a list of numbers with the sum of itself
-- and all preceding elements.
runningSum :: Num a => [a] -> [(a, a)]
runningSum xs = zip xs (scanl1 (+) xs)

-- |Augment each element of a list of arbitrary type with the sum 
-- of itself and all preceding elements as mapped through a function.
runningSum2 :: Num a 
            => (b -> a)     -- ^A function to produce numbers to sum
            -> [b]          -- ^The list to augment
            -> [(b, a)]     -- ^The augmented list
runningSum2 f xs = zip xs (scanl1 (+) (map f xs))

-- |Split a number m >= n into the pair (n, m - n).
splitNum :: (Num a, Ord a, Show a) => a -> a -> (a, a)
splitNum m n
    | m >= n    = (n, m - n)
    | otherwise = error msg
    where
        msg = "splitNum: m=" ++ show m ++ " < n=" ++ show n

-- |Partition a list of numbers into two such that the first sums to
-- a non-negative goal, splitting an element if necessary. If the 
-- original list sums to less than the goal, return it with a null 
-- second cell.
fifoSplit :: (Num a, Ord a, Show a) => a -> [a] -> ([a],[a])
fifoSplit n ns
    | n < 0 = error ("fifoSplit called with negative target: " ++ show n)
    | otherwise = (as,bs)
    where
        sumNoMoreThan n (x, s) = s <= n 
        (xrs,yrs) = span (sumNoMoreThan n) (runningSum ns)
        (dx,dy) = 
            if null yrs
                then (0,0)
                else if null xrs
                    then splitNum yh n
                    else splitNum yh (n - (snd.last) xrs)
        strip xrs = if null xrs then [] else map fst xrs
        yh = (fst.head) yrs
        xs = strip xrs
        ys = strip yrs
        (as,bs) = case (dx,dy) of
                    (0,0) -> (xs      , ys)
                    (0,y) -> (xs      , y : tail ys)
                    (x,0) -> (xs++[x] , tail ys)
                    (x,y) -> (xs++[x] , y : tail ys)

-- |Partition a list into two such that the first, mapped through
-- a function, sums to a non-negative goal, splitting an element
-- if necessary by a copy-and-modify function. If the original list 
-- sums to less than the goal, return it with a null second cell.
fifoSplit2 :: (Num a, Ord a, Show a)
           => a             -- ^A non-negative goal
           -> [b]           -- ^The list to split
           -> (b -> a)      -- ^A function to produce numbers to sum
           -> (b -> a -> b) -- ^A function to copy and modify a list element
           -> ([b],[b])     -- ^The partition

fifoSplit2 n ns f mk
    | n < 0 = error msg
    | otherwise = (as,bs)
    where
        msg = "fifoSplit2 called with negative target: " ++ show n
        (as,bs) = case (dx,dy) of
                    (0,0) -> (xs      , ys)
                    (0,_) -> (xs      , y : tail ys)
                    (_,0) -> (xs++[x] , tail ys)
                    (_,_) -> (xs++[x] , y : tail ys)

        (dx,dy) = case (xrs, yrs) of
                    (_, [])   -> (0,0)
                    ([], _)   -> splitNum (f yh) n
                    otherwise -> splitNum (f yh) (n - (snd.last) xrs)
        (xrs,yrs) = span (sumNoMoreThan n) (runningSum2 f ns)
        yh = (fst.head) yrs
        xs = strip xrs; ys = strip yrs
        x  = mk yh dx;  y  = mk yh dy

        -- sumNoMoreThan = predicate to split running sum just before goal
        -- strip = restore list from running sum
        sumNoMoreThan n (_, s) = s <= n 
        strip xrs = if null xrs then [] else map fst xrs

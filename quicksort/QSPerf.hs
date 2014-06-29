{-# LANGUAGE BangPatterns, RankNTypes #-}
module Main where
 
import Data.Array.ST
import Data.Int
import Data.List
import Control.Monad
import Control.Monad.ST
import Control.DeepSeq
import System.Random
import Data.Time.Clock
import System.Environment
 
-- | Returns the second largest element of three.
-- if x == y, then z is not evaluated.
middle3 :: (Ord a) => a -> a -> a -> Int
middle3 x y z = case compare x y of
    LT -> if y > z then if x < z then 2 else 0 else 1
    GT -> if y < z then if x < z then 0 else 2 else 1
    _  -> 0
 
swap :: STArray s Int a -> Int -> Int -> ST s ()
swap ary x y = do
      l <- readArray ary x
      r <- readArray ary y
      writeArray ary x r
      writeArray ary y l
 
sortInplace :: (Ord a) => STArray s Int a -> Int -> Int -> ST s ()
sortInplace ary !low !high
  | low >= high = return ()
  | low + 1 == high = do
    let cmp x y = do
         l <- readArray ary x
         r <- readArray ary y
         return $ compare l r
    s <- cmp low high
    when (s == GT) $ swap ary low high
  | otherwise = do
    let mid = (low + high) `div` 2
    l <- readArray ary low
    m <- readArray ary mid
    r <- readArray ary high
    let piv = [low,mid,high] !! (middle3 l m r)
    pivVal <- readArray ary piv
    swap ary piv low
    let sub0 !a !b
         | a >= b = sub3 a b
         | otherwise = sub05 a b
        sub05 !a !b 
         | a < high = do
           ai <- readArray ary a
           if (ai <= pivVal) then sub05 (a+1) b else sub1 a b
         | otherwise = sub1 a b
        sub1 !a !b = do
         aj <- readArray ary b
         if aj > pivVal then sub1 a (b-1) else sub2 a b
        sub2 !a !b
         | a < b = (swap ary a b) >> sub0 a b
         | otherwise = sub0 a b
        sub3 !_a !b = do
         swap ary low b
         sortInplace ary low (b-1)
         sortInplace ary (b+1) high
    sub0 low high
qsort1 :: (Ord a) => [a] -> [a]
qsort1 ls = let n = length ls in

  case n of
    0 -> []
    1 -> ls
    _ -> runST $ do
    ary <- newListArray (0,n-1) ls
    sortInplace ary 0 (n-1)
    getElems ary
 
-- | /O(n*log(n))/. Quicksort. Pivot is selected in the following way:
--   a: the first element, b : the element in the middle, c : the last element
--   pivot = the second of {a,b,c}
--   Note that in the worst case, @quickSort ls@ takes /O(n^2)/-time.
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort ls = quickSort l1 ++ l2 ++ quickSort l3 where
  v1 = (head ls)
  v2 = (ls !! (length ls `div` 2))
  v3 = (last ls)
  pivot = [v1,v2,v3] !! (middle3 v1 v2 v3)
  (l1, l2, l3) = divide ls [] [] []
  divide [] p1 p2 p3 = (p1, p2, p3)
  divide (x:xs) p1 p2 p3 = case compare x pivot of
    LT -> divide xs (x:p1) p2 p3
    EQ -> divide xs p1 (x:p2) p3
    GT -> divide xs p1 p2 (x:p3)

hash :: [Int] -> Int64
hash ls = foldl' (\e x -> e * 31373 + fromIntegral x) 0 ls


time :: (forall a. Ord a => [a] -> [a]) -> [Int] -> IO (Int64, NominalDiffTime)
time sorter ls = do
  from <- getCurrentTime
  let srt = sorter ls
  to   <- deepseq srt getCurrentTime
  return $ (hash srt, diffUTCTime to from)
 
-- | /O(n*log(n))/. Mergesort. 
--   This sort is stable.
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort [a,b] = if a<=b then [a,b] else [b,a]
mergeSort ls = merge (mergeSort p1) (mergeSort p2) where
  (p1, p2) = splitAt (length ls `div` 2) ls -- halves the list
  merge [] ls' = ls'
  merge ls' [] = ls'
  merge (x:xs) (y:ys) =
    if x<=y then x : merge xs (y:ys) -- For stability, @merge@ prefers the first list when it encounters equal elements.
       else y : merge (x:xs) ys

summary :: String -> (forall a. Ord a => [a] -> [a]) -> [Int] -> IO ()
summary algName sorter ls = do
   (hashVal, t1) <- time sorter ls
   putStrLn $ algName ++ ":" ++ show t1
   putStrLn $ "  hash: " ++ show hashVal


main :: IO ()
main = do
   args <- getArgs
   let n = read $ if length args >= 1 then args !! 0 else "100000"
   ls <- sequence (replicate n randomIO) :: IO [Int]
   deepseq ls $ summary "qsort1" qsort1 ls
   summary "quickSort" quickSort ls
   summary "mergeSort" mergeSort ls
   ls2 <- sequence (replicate n randomIO) :: IO [Int]
   deepseq ls2 $ summary "mergeSort" mergeSort ls2
   summary "quickSort" quickSort ls2
   summary "qsort1" qsort1 ls2


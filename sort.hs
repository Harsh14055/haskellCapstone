import Data.List (sort)
import System.Random (randomRs, newStdGen)
import Control.Parallel.Strategies (parMap, rdeepseq, using, rseq)
import Control.DeepSeq (NFData, deepseq)

-- Merge Sort Implementation
mergeSort :: (Ord a) => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

-- QuickSort Implementation
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y < x] ++ [x] ++ quickSort [y | y <- xs, y >= x]

-- Bubble Sort Implementation
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort xs = case bubble xs of
    (swapped, xs') -> if swapped then bubbleSort xs' else xs'
  where
    bubble [] = (False, [])
    bubble [x] = (False, [x])
    bubble (x:y:xs)
      | x > y     = let (swapped, rest) = bubble (x:xs) in (True, y:rest)
      | otherwise = let (swapped, rest) = bubble (y:xs) in (swapped, x:rest)

-- Parallel Merge Sort using parMap
parallelMergeSort :: (Ord a, NFData a) => [a] -> [a]
parallelMergeSort []  = []
parallelMergeSort [x] = [x]
parallelMergeSort xs  = merge sortedLeft sortedRight
  where
    (left, right) = splitAt (length xs `div` 2) xs
    sortedLeft = parallelMergeSort left `using` rseq
    sortedRight = parallelMergeSort right `using` rseq
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

-- Function to compare sorting performance
timeSorting :: ([Int] -> [Int]) -> IO ()
timeSorting sortFunc = do
    gen <- newStdGen
    let randomList = take 10000 (randomRs (1, 100000) gen :: [Int])
    print $ take 10 (sortFunc randomList) -- Print first 10 sorted elements

-- Custom Sorting Order
customSort :: (a -> a -> Bool) -> [a] -> [a]
customSort cmp [] = []
customSort cmp (x:xs) = 
    customSort cmp [y | y <- xs, cmp y x] ++ [x] ++ customSort cmp [y | y <- xs, not (cmp y x)]

main :: IO ()
main = do
    let testList :: [Int]  -- Explicitly define type
        testList = [5, 3, 8, 6, 2, 7, 4, 1]
    
    putStrLn "Merge Sort: "
    print (mergeSort testList)
    
    putStrLn "QuickSort: "
    print (quickSort testList)
    
    putStrLn "Bubble Sort: "
    print (bubbleSort testList)
    
    putStrLn "Parallel Merge Sort: "
    print (parallelMergeSort testList)
    
    putStrLn "Custom Sort (Descending Order): "
    print (customSort (>) testList)
    
    putStrLn "Performance Test (Merge Sort): "
    timeSorting mergeSort

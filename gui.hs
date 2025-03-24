{-# LANGUAGE OverloadedStrings #-}

import Graphics.UI.Gtk
import Control.Monad.IO.Class (liftIO)
import Control.Parallel.Strategies (using, rseq)
import Control.DeepSeq (NFData, force)
import qualified Data.Text as T
import Data.Text (Text)
import Data.List (sort)
import System.Random (randomRs, newStdGen)

-- Merge Sort
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

-- QuickSort
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y < x] ++ [x] ++ quickSort [y | y <- xs, y >= x]

-- Bubble Sort
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort xs = case bubble xs of
    (swapped, xs') -> if swapped then bubbleSort xs' else xs'
  where
    bubble [] = (False, [])
    bubble [x] = (False, [x])
    bubble (x:y:xs)
      | x > y     = let (swapped, rest) = bubble (x:xs) in (True, y:rest)
      | otherwise = let (swapped, rest) = bubble (y:xs) in (swapped, x:rest)

-- Parallel Merge Sort
parallelMergeSort :: (Ord a, NFData a) => [a] -> [a]
parallelMergeSort []  = []
parallelMergeSort [x] = [x]
parallelMergeSort xs  = merge sortedLeft sortedRight
  where
    (left, right) = splitAt (length xs `div` 2) xs
    sortedLeft = force (parallelMergeSort left) `using` rseq
    sortedRight = force (parallelMergeSort right) `using` rseq
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

-- Custom Sort (Descending Order)
customSort :: (a -> a -> Bool) -> [a] -> [a]
customSort cmp [] = []
customSort cmp (x:xs) = 
    customSort cmp [y | y <- xs, cmp y x] ++ [x] ++ customSort cmp [y | y <- xs, not (cmp y x)]

-- Function to parse input string into a list of numbers
parseInput :: T.Text -> [Int]
parseInput input = map (read . T.unpack) (T.words input)

main :: IO ()
main = do
    -- Initialize GTK
    _ <- initGUI
    window <- windowNew
    set window [ windowTitle := T.pack "Sorting Visualizer"
               , containerBorderWidth := 10
               , windowDefaultWidth := 400
               , windowDefaultHeight := 300 ]

    -- Create layout
    vbox <- vBoxNew False 10
    containerAdd window vbox

    -- Entry field for input
    inputEntry <- entryNew
    boxPackStart vbox inputEntry PackGrow 5

    -- Dropdown for sorting algorithms
    comboBox <- comboBoxNewText
    mapM_ (comboBoxAppendText comboBox . T.pack) 
        ["Merge Sort", "QuickSort", "Bubble Sort", "Parallel Merge Sort", "Custom Sort (Descending)"]
    boxPackStart vbox comboBox PackGrow 5

    -- Button to trigger sorting
    button <- buttonNewWithLabel (T.pack "Sort")
    boxPackStart vbox button PackGrow 5

    -- Label for displaying output
    outputLabel <- labelNew (Just (T.pack "Sorted Output: "))
    boxPackStart vbox outputLabel PackGrow 5

    -- Sorting logic when button is clicked
    button `on` buttonActivated $ do
        inputText <- entryGetText inputEntry
        selectedIndex <- comboBoxGetActive comboBox

        let numbers = parseInput (T.pack inputText)
            sortedNumbers = case selectedIndex of
                0 -> mergeSort numbers
                1 -> quickSort numbers
                2 -> bubbleSort numbers
                3 -> parallelMergeSort numbers
                4 -> customSort (>) numbers
                _ -> numbers  -- Default case

        labelSetText outputLabel (T.pack ("Sorted Output: " ++ unwords (map show sortedNumbers)))

    -- Close window handler
    window `on` deleteEvent $ do
        liftIO mainQuit
        return False

    -- Show GUI
    widgetShowAll window
    mainGUI

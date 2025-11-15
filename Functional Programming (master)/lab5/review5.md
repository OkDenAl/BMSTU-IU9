% Лабораторная работа № 5. Решение задач на языке Haskell:
  программирование с побочными эффектами
% 13 ноября 2025 г.
% Денис Окутин, ИУ9-11М

# Цель работы
Получение навыков вычислений в монадах на языке программирования Haskell.

Реализуйте один из алгоритмов сортировки списка (см. индивидуальный вариант), 
например, быстрой, выбором, вставками.
Пусть функции сортировки принимают два аргумента: предикат для сравнения элементов и исходный список.
В реализациях не обращайтесь к элементу списка по его индексу. При необходимости используйте конструкции языка
Haskell для включения элементов в список (списковые включения, list comprehension). Оцените вычислительную
сложность. Экспериментально определите время, затрачиваемое вашими функциями на сортировку списков
различной длины. Чем вы можете объяснить низкую эффективность полученных реализаций?

Предложите альтернативные реализации тех же алгоритмов сортировки на основе модулей Data.Array из
стандартной библиотеки Haskell Platform или Data.Vector из кодовой базы Hackage. Снова определите время,
затрачиваемое затрачиваемое полученными функциями на сортировку списков различной длины. 
Достигнуто ли повышение производительности?

# Индивидуальный вариант
Быстрая сортировка.

# Реализация и тестирование

```haskell
import Data.Array
import Control.Monad
import Control.Exception (evaluate)
import Data.Time.Clock
import Data.List (sort , foldl')

import Control.Monad.ST
import Data.STRef
import Data.Array.ST

import Data.Array.IO
import Data.IORef

import System.IO.Unsafe (unsafePerformIO)

-----------------------------------------------------------------------

quickSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
quickSort _ [] = []
quickSort cmp (x:xs) = 
    let left = quickSort cmp [y | y <- xs, cmp y x]
        right = quickSort cmp [y | y <- xs, not (cmp y x)]
    in left ++ [x] ++ right

-----------------------------------------------------------------------

quickSortArray1 :: (Int -> Int -> Bool) -> [Int] -> [Int]
quickSortArray1 cmp xs = elems $ qsort arr
  where
    arr = listArray (0, length xs - 1) xs

    qsort :: Array Int Int -> Array Int Int
    qsort a
      | n <= 0    = a
      | otherwise =
          let pivot = a ! 0
              (leftList, rightList) = partitionArray cmp pivot a
              leftArr  = qsort (listArray (0, length leftList - 1) leftList)
              rightArr = qsort (listArray (0, length rightList - 1) rightList)
          in listArray (0, length a - 1) (elems leftArr ++ [pivot] ++ elems rightArr)
      where
        (_, n') = bounds a
        n = n'

    partitionArray :: (Int -> Int -> Bool) -> Int -> Array Int Int -> ([Int], [Int])
    partitionArray cmp pivot a =
      let vals = [a ! i | i <- indices a, i /= 0]
      in ([x | x <- vals, cmp x pivot],
          [x | x <- vals, not (cmp x pivot)])

-----------------------------------------------------------------------

quickSortArray :: (Int -> Int -> Bool) -> [Int] -> [Int]
quickSortArray cmp xs = 
    let n = length xs
        arr = listArray (1, n) xs
        sortedIndices = quickSortIndices cmp arr [1..n]
    in map (arr !) sortedIndices

quickSortIndices :: (Int -> Int -> Bool) -> Array Int Int -> [Int] -> [Int]
quickSortIndices _ _ [] = []
quickSortIndices cmp arr indices =
    let
        midIndex = indices !! (length indices `div` 2)
        firstIndex = head indices
        lastIndex = last indices
        pivotIndex = medianOfThree1 arr firstIndex midIndex lastIndex
        pivotValue = arr ! pivotIndex
        remainingIndices = filter (/= pivotIndex) indices
        (left, equal, right) = partitionIndices cmp arr pivotValue remainingIndices
        sortedLeft = quickSortIndices cmp arr left
        sortedRight = quickSortIndices cmp arr right
    in sortedLeft ++ equal ++ [pivotIndex] ++ sortedRight

medianOfThree1 :: Array Int Int -> Int -> Int -> Int -> Int
medianOfThree1 arr a b c =
    let x = arr ! a
        y = arr ! b  
        z = arr ! c
    in if (x <= y && y <= z) || (z <= y && y <= x) then b
       else if (y <= x && x <= z) || (z <= x && x <= y) then a
       else c

partitionIndices :: (Int -> Int -> Bool) -> Array Int Int -> Int -> [Int] -> ([Int], [Int], [Int])
partitionIndices cmp arr pivotValue indices =
    let (left, equal, right) = foldl' classify ([], [], []) indices
        classify (l, e, r) i
            | val == pivotValue = (l, i:e, r)
            | cmp val pivotValue = (i:l, e, r)
            | otherwise = (l, e, i:r)
            where val = arr ! i
    in (left, equal, right)

------------------------------------------------------------

inPlaceQuickSortST :: (Int -> Int -> Bool) -> [Int] -> [Int]
inPlaceQuickSortST cmp xs = runST $ do
    let n = length xs
    arr <- newListArray (0, n - 1) xs :: ST s (STArray s Int Int)
    quicksortST arr 0 (n - 1)
    getElems arr

quicksortST :: STArray s Int Int -> Int -> Int -> ST s ()
quicksortST arr lo hi =
    when (lo < hi) $ do
        pivotIdx <- medianOfThreeST arr lo hi
        swapST arr pivotIdx hi
        p <- partitionST arr lo hi
        quicksortST arr lo (p - 1)
        quicksortST arr (p + 1) hi

partitionST :: STArray s Int Int -> Int -> Int -> ST s Int
partitionST arr lo hi = do
    pivot <- readArray arr hi
    iRef <- newSTRef lo
    forM_ [lo .. hi - 1] $ \j -> do
        v <- readArray arr j
        i <- readSTRef iRef
        when (v <= pivot) $ do
            swapST arr i j
            writeSTRef iRef (i + 1)
    i <- readSTRef iRef
    swapST arr i hi
    return i

medianOfThreeST :: STArray s Int Int -> Int -> Int -> ST s Int
medianOfThreeST arr lo hi = do
    let mid = (lo + hi) `div` 2
    a <- readArray arr lo
    b <- readArray arr mid
    c <- readArray arr hi
    return $
      if (a <= b && b <= c) || (c <= b && b <= a) then mid
      else if (b <= a && a <= c) || (c <= a && a <= b) then lo
      else hi

swapST :: STArray s Int Int -> Int -> Int -> ST s ()
swapST arr i j = when (i /= j) $ do
    vi <- readArray arr i
    vj <- readArray arr j
    writeArray arr i vj
    writeArray arr j vi

------------------------------------------------------------

inPlaceQuickSortIOWrapper :: (Int -> Int -> Bool) -> [Int] -> [Int]
inPlaceQuickSortIOWrapper cmp xs = unsafePerformIO (inPlaceQuickSortIO cmp xs)

inPlaceQuickSortIO :: (Int -> Int -> Bool) -> [Int] -> IO [Int]
inPlaceQuickSortIO cmp xs = do
    let n = length xs
    arr <- newListArray (0, n - 1) xs :: IO (IOArray Int Int)
    quicksortIO cmp arr 0 (n - 1)
    getElems arr

quicksortIO :: (Int -> Int -> Bool) -> IOArray Int Int -> Int -> Int -> IO ()
quicksortIO cmp arr lo hi =
    when (lo < hi) $ do
        pivotIdx <- medianOfThreeIO arr lo hi
        swapIO arr pivotIdx hi
        p <- partitionIO cmp arr lo hi
        quicksortIO cmp arr lo (p - 1)
        quicksortIO cmp arr (p + 1) hi

partitionIO :: (Int -> Int -> Bool) -> IOArray Int Int -> Int -> Int -> IO Int
partitionIO cmp arr lo hi = do
    pivot <- readArray arr hi
    iRef <- newIORef lo
    forM_ [lo .. hi - 1] $ \j -> do
        v <- readArray arr j
        i <- readIORef iRef
        when (cmp v pivot) $ do
            swapIO arr i j
            writeIORef iRef (i + 1)
    i <- readIORef iRef
    swapIO arr i hi
    return i

medianOfThreeIO :: IOArray Int Int -> Int -> Int -> IO Int
medianOfThreeIO arr lo hi = do
    let mid = (lo + hi) `div` 2
    a <- readArray arr lo
    b <- readArray arr mid
    c <- readArray arr hi
    return $
      if (a <= b && b <= c) || (c <= b && b <= a) then mid
      else if (b <= a && a <= c) || (c <= a && a <= b) then lo
      else hi

swapIO :: IOArray Int Int -> Int -> Int -> IO ()
swapIO arr i j = when (i /= j) $ do
    vi <- readArray arr i
    vj <- readArray arr j
    writeArray arr i vj
    writeArray arr j vi


----------------- Тестирование ---------------------

simpleRandom :: Int -> [Int]
simpleRandom seed = tail $ iterate next seed
  where
    next x = (1103515245 * x + 12345) `mod` (2^31)

generateDeterministicList :: Int -> Int -> [Int]
generateDeterministicList seed n = 
    take n $ map (`mod` (n * 10)) (simpleRandom seed)


prepareTestData :: [Int] -> IO [(Int, [Int], [Int])]
prepareTestData sizes = do
    putStrLn "Генерация тестовых данных..."
    let testData = map (\n -> 
            let seed = n * 42
                randomList = generateDeterministicList seed n
                -- randomList = reverse [0..n]
                expected = sort randomList
            in (n, randomList, expected)) sizes
    return testData

testPerformanceWithName :: String -> ((Int -> Int -> Bool) -> [Int] -> [Int]) -> [(Int, [Int], [Int])] -> IO ()
testPerformanceWithName name sortFunc testData = do
    putStrLn $ "\nТестируется: " ++ name
    forM_ testData $ \(n, inputList, expected) -> do
        putStr $ "  Размер " ++ show n ++ ": "
        
        start <- getCurrentTime
        let sorted = sortFunc (<=) inputList
        evaluate (length sorted)
        end <- getCurrentTime
        
        let time = diffUTCTime end start
        let isCorrect = sorted == expected
        
        putStrLn $ show time ++ 
                  if isCorrect
                  then " Корректно"
                  else " ОШИБКА!" ++ show inputList ++ show sorted ++ show expected


-- ghc -O2 -rtsopts lab5.hs
-- ./lab5 +RTS -s

main :: IO ()
main = do
    putStrLn "\n=== Сравнение алгоритмов сортировки на случайных данных ==="
    
    let sizes = [1000, 10000, 100000]
    testData <- prepareTestData sizes

 -------------------------------------------------------------------------------   
    testPerformanceWithName "Быстрая сортировка (списки)" quickSort testData
 
    {- Тестируется: Быстрая сортировка (списки)
    Размер 1000: 0.000277s Корректно
    Размер 10000: 0.004719s Корректно
    Размер 100000: 0.068468s Корректно
        306,703,632 bytes allocated in the heap
        87,556,152 bytes copied during GC
        9,841,864 bytes maximum residency (7 sample(s))
            64,048 bytes maximum slop
                35 MiB total memory in use (0 MiB lost due to fragmentation)

                                        Tot time (elapsed)  Avg pause  Max pause
    Gen  0        67 colls,     0 par    0.034s   0.035s     0.0005s    0.0020s
    Gen  1         7 colls,     0 par    0.023s   0.025s     0.0036s    0.0073s

    TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

    SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

    INIT    time    0.004s  (  0.004s elapsed)
    MUT     time    0.090s  (  0.087s elapsed)
    GC      time    0.056s  (  0.060s elapsed)
    EXIT    time    0.001s  (  0.010s elapsed)
    Total   time    0.152s  (  0.162s elapsed)

    Alloc rate    3,391,651,262 bytes per MUT second

    Productivity  59.6% of total user, 54.0% of total elapsed -}

-------------------------------------------------------------------------------
    testPerformanceWithName "Быстрая сортировка (Data.Array1)" quickSortArray1 testData

    {- Тестируется: Быстрая сортировка (Data.Array1)
    Размер 1000: 0.000858s Корректно
    Размер 10000: 0.014117s Корректно
    Размер 100000: 0.121349s Корректно
        561,872,288 bytes allocated in the heap
        114,988,144 bytes copied during GC
        11,184,880 bytes maximum residency (9 sample(s))
            71,904 bytes maximum slop
                38 MiB total memory in use (0 MiB lost due to fragmentation)

                                        Tot time (elapsed)  Avg pause  Max pause
    Gen  0       122 colls,     0 par    0.049s   0.052s     0.0004s    0.0021s
    Gen  1         9 colls,     0 par    0.028s   0.033s     0.0036s    0.0073s

    TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

    SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

    INIT    time    0.005s  (  0.005s elapsed)
    MUT     time    0.122s  (  0.116s elapsed)
    GC      time    0.078s  (  0.084s elapsed)
    EXIT    time    0.001s  (  0.002s elapsed)
    Total   time    0.205s  (  0.208s elapsed)

    Alloc rate    4,612,088,454 bytes per MUT second

    Productivity  59.3% of total user, 55.8% of total elapsed -}

-------------------------------------------------------------------------------
    testPerformanceWithName "Быстрая сортировка (Data.Array)" quickSortArray testData

    {- Тестируется: Быстрая сортировка (Data.Array)
    Размер 1000: 0.000314s Корректно
    Размер 10000: 0.005031s Корректно
    Размер 100000: 0.073676s Корректно
        247,987,312 bytes allocated in the heap
        91,320,888 bytes copied during GC
        12,828,456 bytes maximum residency (7 sample(s))
            105,744 bytes maximum slop
                39 MiB total memory in use (0 MiB lost due to fragmentation)

                                        Tot time (elapsed)  Avg pause  Max pause
    Gen  0        52 colls,     0 par    0.034s   0.035s     0.0007s    0.0021s
    Gen  1         7 colls,     0 par    0.021s   0.023s     0.0033s    0.0083s

    TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

    SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

    INIT    time    0.004s  (  0.004s elapsed)
    MUT     time    0.096s  (  0.093s elapsed)
    GC      time    0.055s  (  0.059s elapsed)
    EXIT    time    0.001s  (  0.004s elapsed)
    Total   time    0.155s  (  0.160s elapsed)

    Alloc rate    2,588,188,822 bytes per MUT second

    Productivity  61.6% of total user, 57.9% of total elapsed -}

-------------------------------------------------------------------------------
    testPerformanceWithName "Быстрая сортировка (Data.Array.ST)" inPlaceQuickSortST testData
    
    {-Тестируется: Быстрая сортировка (Data.Array.ST)
    Размер 1000: 0.000161s Корректно
    Размер 10000: 0.002727s Корректно
    Размер 100000: 0.034076s Корректно
        223,512,616 bytes allocated in the heap
        65,221,472 bytes copied during GC
        9,522,016 bytes maximum residency (6 sample(s))
            66,720 bytes maximum slop
                32 MiB total memory in use (0 MiB lost due to fragmentation)

                                        Tot time (elapsed)  Avg pause  Max pause
    Gen  0        47 colls,     0 par    0.027s   0.028s     0.0006s    0.0023s
    Gen  1         6 colls,     0 par    0.017s   0.019s     0.0032s    0.0068s

    TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

    SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

    INIT    time    0.003s  (  0.003s elapsed)
    MUT     time    0.067s  (  0.064s elapsed)
    GC      time    0.044s  (  0.047s elapsed)
    EXIT    time    0.001s  (  0.007s elapsed)
    Total   time    0.114s  (  0.122s elapsed)

    Alloc rate    3,324,299,720 bytes per MUT second

    Productivity  58.8% of total user, 53.0% of total elapsed-}

-------------------------------------------------------------------------------
    testPerformanceWithName "Быстрая сортировка (Data.Array.IO)" inPlaceQuickSortIOWrapper testData

    {- === Сравнение алгоритмов сортировки на случайных данных ===
    Генерация тестовых данных...

    Тестируется: Быстрая сортировка (Data.Array.IO)
    Размер 1000: 0.000177s Корректно
    Размер 10000: 0.002614s Корректно
    Размер 100000: 0.050441s Корректно
        148,796,488 bytes allocated in the heap
        60,909,400 bytes copied during GC
        9,521,648 bytes maximum residency (6 sample(s))
            62,992 bytes maximum slop
                32 MiB total memory in use (0 MiB lost due to fragmentation)

                                        Tot time (elapsed)  Avg pause  Max pause
    Gen  0        29 colls,     0 par    0.024s   0.024s     0.0008s    0.0026s
    Gen  1         6 colls,     0 par    0.019s   0.022s     0.0036s    0.0080s

    TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

    SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

    INIT    time    0.004s  (  0.004s elapsed)
    MUT     time    0.084s  (  0.081s elapsed)
    GC      time    0.042s  (  0.046s elapsed)
    EXIT    time    0.001s  (  0.004s elapsed)
    Total   time    0.131s  (  0.135s elapsed)

    Alloc rate    1,770,122,388 bytes per MUT second

    Productivity  64.1% of total user, 60.2% of total elapsed=
    -}
```

# Вывод
В ходе выполнения лабораторной работы была реализована быстрая сортировка с использованием
различных возможностей языка Haskell. Помимо этого также был создан модуль тестирования,
который генерирует псевдослучайные списки заданной длины для их будущей сортировки.

Помимо основной задачи была также сделана ачивка - было проведено сравнение 
Data.Array с Data.IOArray и Data.STArray. И были получены ожидаемые результаты.

Про результаты нужно рассказать подробнее, так как при обычном запуске по кнопке в 
IDE я видел странную картину - реализация на списках в общем случае работала быстрее, 
чем все остальные. Однако при компиляции с оптимизацией и профилированием `ghc -O2 -rtsopts lab5.hs`
я получил уже то, что ожидалось получить. 

Эффективнее всего по памяти и времени работы
оказались варианты реализации через Data.IOArray и Data.STArray, это ожидаемо, так как
нет оверхеда на создание каждый раз нового списка при изменениях, потому что в
таком варианте мы имеем возможность делать in-place сортирвоку.

В ходе работы было проведено настоящее исследование, это было интересно делать!
----- 1
arith :: (Integer -> Bool) -> (Integer, Integer, Integer) -> [Integer]
arith predicate (n, a0, d) = 
    take (fromIntegral n) $ filter predicate $ iterate (\x -> x + d) a0

-----2
kadane :: [Integer] -> (Integer, Integer)
kadane xs = 
    let 
        (maxSum, _, _, start, end) = foldl' step (0, 0, 0, 0, -1) (zip [0..] xs)
        step (maxSum, currentSum, currentStart, bestStart, bestEnd) (i, x) =
            let newSum = currentSum + x
                (newCurrentSum, newCurrentStart) = 
                    if newSum < 0 
                    then (0, i + 1) 
                    else (newSum, currentStart)
                (newMaxSum, newBestStart, newBestEnd) = 
                    if newCurrentSum > maxSum 
                    then (newCurrentSum, newCurrentStart, i)
                    else (maxSum, bestStart, bestEnd)
            in (newMaxSum, newCurrentSum, newCurrentStart, newBestStart, newBestEnd)
    in if maxSum == 0 
       then (0, -1)
       else (start, end)
    
----- 3
power :: Integer -> Integer -> Integer
power base exponent
    | exponent < 0 = error "Negative exponent not supported"
    | exponent == 0 = 1
    | exponent == 1 = base
    | even exponent = let half = power base (exponent `div` 2) in half * half
    | otherwise = base * power base (exponent - 1)

----- 4
sublists :: [Integer] -> Integer -> [[Integer]]
sublists [] _ = []
sublists xs maxSum = 
    let (chunk, rest) = splitChunk xs maxSum 0 []
    in chunk : sublists rest maxSum
  where
    splitChunk [] _ _ acc = (reverse acc, [])
    splitChunk (x:xs) maxSum currentSum acc
        | currentSum + x <= maxSum = 
            splitChunk xs maxSum (currentSum + x) (x:acc)
        | null acc = 
            ([x], xs)
        | otherwise = 
            (reverse acc, x:xs)



main = do
    ----- 1
    let result1 = arith even (5, 2, 3)
    putStrLn ("Результат 1: " ++ show result1)
    ----- 2
    let result2 = kadane [1, -3, 4, -1, 2, 1, -5, 4] 
    putStrLn ("Результат 2: " ++ show result2)
    ----- 3
    let result3 = power 2 10
    putStrLn ("Результат 3: " ++ show result3)
    ----- 4
    let result4 = sublists [1,2,3,4,5,6,7,8,9,10] 15
    putStrLn ("Результат 4: " ++ show result4)



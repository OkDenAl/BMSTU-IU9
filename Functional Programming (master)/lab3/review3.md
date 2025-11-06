% Лабораторная работа № 3. Решение задач на языке Haskell: программирование без побочных эффектов
% 16 октября 2025 г.
% Денис Окутин, ИУ9-11М

# Цель работы
Получение навыков составления программ на языке Haskell.

# Индивидуальный вариант
1. Чистые функции, работающие со списком
   Функция

`arith :: (Integer -> Boolean) -> (Integer, Integer, Integer) -> [Integer]`
возвращающая список из n первых членов арифметической прогрессии с начальным членом a0 и
разностью d, удовлетворяющих некоторому предикату.

Функция `kadane :: [Integer] -> (Integer, Integer)`, выполняющая поиск границ подпоследовательности с
максимальной суммой (алгоритм Кадана).

Функция `power :: Integer -> Integer -> Integer`, выполняющая быстрое возведение числа в указанную степень
(параметр функции — степень).

Функция `sublists :: [Integer] -> Integer -> [[Integer]]`, разбивающая список целых чисел на
непересекающиеся подсписки, сумма элементов которых не превышает указанного числа. 
Подсписком будем считать список,
который можно получить удалением произвольного количества элементов от начала и от конца списка.

2. Алгебраический тип данных
   Абстрактный синтаксис арифметических выражений:

```
Expr → Expr + Expr | Expr - Expr | Expr * Expr | Expr / Expr
| Expr ^ Expr | NUMBER | VARNAME
```


Требуется написать функцию `asString :: Expr -> String`, записывающую выражение в текстовой форме с
минимумом круглых скобок. Считаем, что наивысшим приоритетом обладает возведение в степень ^,
затем умножение * и деление /, затем сложение + и вычитание -. Степень правоассоциативна,
остальные операции левоассоциативны.

Допустимо вместо написания функции asString реализовать класс Show.

# Реализация

```haskell
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
            
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Number Integer
          | Var String
          deriving (Eq)

instance Show Expr where
  show = asString
    where
      asString expr = showExpr 0 False expr
        where
          precedence (Add _ _) = 1
          precedence (Sub _ _) = 1
          precedence (Mul _ _) = 2
          precedence (Div _ _) = 2
          precedence (Pow _ _) = 3
          precedence _          = 4
          
          isLeftAssoc (Pow _ _) = False
          isLeftAssoc _         = True
          
          showExpr parentPrec isRight expr =
            let currentPrec = precedence expr
                needParens = currentPrec < parentPrec ||
                            (currentPrec == parentPrec && isRight && isLeftAssoc expr) ||
                            (currentPrec == parentPrec && not isRight && not (isLeftAssoc expr))
                str = case expr of
                       Add e1 e2 -> showExpr currentPrec False e1 ++ " + " \
                       ++ showExpr currentPrec True e2
                       Sub e1 e2 -> showExpr currentPrec False e1 ++ " - " \
                       ++ showExpr currentPrec True e2
                       Mul e1 e2 -> showExpr currentPrec False e1 ++ " * " \
                       ++ showExpr currentPrec True e2
                       Div e1 e2 -> showExpr currentPrec False e1 ++ " / " \
                       ++ showExpr currentPrec True e2
                       Pow e1 e2 -> showExpr currentPrec False e1 ++ " ^ " \
                       ++ showExpr currentPrec True e2
                       Number n  -> show n
                       Var name  -> name
            in if needParens then "(" ++ str ++ ")" else str
```

# Тестирование

```
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

expr1 = Add (Number 1) (Number 2)                           -- 1 + 2
expr2 = Mul (Add (Number 1) (Number 2)) (Number 3)          -- (1 + 2) * 3
expr3 = Add (Number 1) (Mul (Number 2) (Number 3))          -- 1 + 2 * 3
expr4 = Pow (Number 2) (Pow (Number 3) (Number 4))          -- 2 ^ 3 ^ 4
expr5 = Pow (Pow (Number 2) (Number 3)) (Number 4)          -- (2 ^ 3) ^ 4
expr6 = Sub (Number 1) (Sub (Number 2) (Number 3))          -- 1 - (2 - 3)
expr7 = Add (Var "x") (Mul (Number 2) (Var "y"))            -- x + 2 * y
expr8 = Div (Mul (Add (Var "a") (Var "b")) (Pow (Var "c") (Var "d"))) (Var "e") -- (a + b) * c ^ d / e

main :: IO ()
main = do  
    putStrLn "Примеры использования класса Show для Expr:"
    
    putStrLn "1. Простое сложение:"
    print expr1  -- Автоматически использует show
    
    putStrLn "2. Сложение с умножением (скобки нужны):"
    print expr2
    
    putStrLn "3. Сложение с умножением (скобки не нужны):"
    print expr3
    
    putStrLn "4. Правоассоциативная степень:"
    print expr4
    
    putStrLn "5. Левоассоциативная степень (явные скобки):"
    print expr5

    putStrLn "6. Вычитание с ассоциативностью:"
    print expr6
    
    putStrLn "7. Выражение с переменными:"
    print expr7
    
    putStrLn "8. Комплексное выражение:"
    print expr8
```

# Вывод
В ходе данной работы были реализованы функции на языке Haskell. Это были первые
мои программы, реализованные на Haskell и уже с практической точки зрения
могу отметить, что в отличие от Scheme Haskell представляет собой более сильный
аппарат для написания программ, это можно увидеть хотя бы посмотрев на реализацию
алгебраического типа данных в scheme и Haskell. Язык интересный, посмотрим, что
будет дальше!
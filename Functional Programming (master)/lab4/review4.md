% Лабораторная работа № 4. Интерпретатор алгоритмического языка программирования
% 6 ноября 2025 г.
% Денис Окутин, ИУ9-11М

# Цель работы
Получение опыта применения функционального программирования для реализации
интерпретатора алгоримического языка программирования.

# Реализация и тестирование

```haskell
module ForthInterpreter where

import Data.List (words, intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

data Env = Env
  { ws :: [String]
  , counter :: Int
  , stack :: [Int]
  , callStack :: [(Int, [String], [LoopFrame], Int)]
  , funcs :: Map String [String]
  , skipDepth :: Int
  , loopStack :: [LoopFrame]
  , output :: [String]
  } deriving (Show)

data LoopFrame = LoopFrame
  { startIndex :: Int
  , endIndex :: Int
  , currentIndex :: Int
  , loopStart :: Int
  , leaveFlag :: Bool
  } deriving (Show)

initialEnv :: [String] -> [Int] -> Env
initialEnv programTokens initialStack = Env
  { ws = programTokens
  , counter = 0
  , stack = initialStack
  , callStack = []
  , funcs = Map.empty
  , skipDepth = 0
  , loopStack = []
  , output = []
  }

step :: Env -> Env
step env 
  | skipDepth env > 0 = skipCode env
  | otherwise =
    let token = ws env !! counter env
        newEnv = env { counter = counter env + 1 }
    in case token of
        "define" -> processDefine newEnv
        "end" -> processEnd newEnv

        t | isNumber t -> newEnv { stack = read t : stack newEnv }

        "+" -> binOp (+) newEnv
        "-" -> binOp (-) newEnv
        "*" -> binOp (*) newEnv
        "/" -> binOp div newEnv
        "mod" -> binOp mod newEnv
        "neg" -> unOp negate newEnv

        "dup" -> dup newEnv
        "drop" -> dropOp newEnv
        "swap" -> swapOp newEnv
        "over" -> overOp newEnv

        "=" -> binOp (\x y -> if x == y then -1 else 0) newEnv
        "<" -> binOp (\x y -> if x < y then -1 else 0) newEnv
        ">" -> binOp (\x y -> if x > y then -1 else 0) newEnv

        "and" -> binOp (\x y -> if x /= 0 && y /= 0 then -1 else 0) newEnv
        "or" -> binOp (\x y -> if x /= 0 || y /= 0 then -1 else 0) newEnv
        "not" -> unOp (\x -> if x == 0 then -1 else 0) newEnv

        "if" -> processIf newEnv
        "endif" -> processEndif newEnv
        "else" -> processElse newEnv
        "do" -> processDo newEnv
        "loop" -> processLoop newEnv
        "i" -> processI newEnv
        "leave" -> processLeave newEnv

        "." -> processDot newEnv
        ".\"" -> processDotString newEnv
        "cr" -> processCr newEnv
        "space" -> processSpace newEnv
        "emit" -> processEmit newEnv

        _ -> case Map.lookup token (funcs newEnv) of
                Just body -> executeUserWord token body newEnv
                Nothing -> error $ "Unknown word: " ++ token

isNumber :: String -> Bool
isNumber s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _ -> False

binOp :: (Int -> Int -> Int) -> Env -> Env
binOp op env = 
  case stack env of
    y:x:xs -> env { stack = op x y : xs }
    _ -> error "Stack underflow in binOp"
 
unOp :: (Int -> Int) -> Env -> Env
unOp op env = 
  case stack env of
    x:xs -> env { stack = op x : xs }
    _ -> error "Stack underflow in unOp"

dup :: Env -> Env
dup env = 
  case stack env of
    x:xs -> env { stack = x:x:xs }
    _ -> error "Stack underflow in dup"

dropOp :: Env -> Env
dropOp env = 
  case stack env of
    _:xs -> env { stack = xs }
    _ -> error "Stack underflow in drop"

swapOp :: Env -> Env
swapOp env = 
  case stack env of
    x:y:xs -> env { stack = y:x:xs }
    _ -> error "Stack underflow in swap"

overOp :: Env -> Env
overOp env = 
  case stack env of
    x:y:xs -> env { stack = y:x:y:xs }
    _ -> error "Stack underflow in over"

processDefine :: Env -> Env
processDefine env = 
  let remaining = drop (counter env) (ws env)
  in if length remaining < 2
     then error "Incomplete word definition"
     else let name = head remaining
              (body, rest) = extractDefinition (tail remaining)
              newFuncs = Map.insert name body (funcs env)
              newCounter = counter env + length body + 2
          in env { funcs = newFuncs, counter = newCounter }

extractDefinition :: [String] -> ([String], [String])
extractDefinition [] = error "Expected 'end'"
extractDefinition ("end":rest) = ([], rest)
extractDefinition (word:words) = 
  let (body, rest) = extractDefinition words
  in (word:body, rest)

processEnd :: Env -> Env
processEnd env = env

executeUserWord :: String -> [String] -> Env -> Env
executeUserWord name body env = 
  let newCallStack = (counter env, ws env, loopStack env, skipDepth env) : callStack env
      newEnv = env { ws = body, counter = 0, callStack = newCallStack, loopStack = [], skipDepth = 0 }
  in interp newEnv

skipCode :: Env -> Env
skipCode env = 
  let token = ws env !! counter env
      newEnv = env { counter = counter env + 1 }
  in case token of
      "if" -> skipCode $ newEnv { skipDepth = skipDepth newEnv + 1 }
      "endif" -> 
        if skipDepth newEnv == 1 
        then newEnv { skipDepth = 0 }
        else skipCode $ newEnv { skipDepth = skipDepth newEnv - 1 }
      "else" -> 
        if skipDepth newEnv == 1 
        then newEnv { skipDepth = 0 }
        else skipCode newEnv
      ".\"" -> skipDotString newEnv
      _ -> skipCode newEnv

skipDotString :: Env -> Env
skipDotString env = 
  let remaining = drop (counter env) (ws env)
      (_, consumed) = extractStringTokens remaining
  in env { counter = counter env + consumed }

processIf :: Env -> Env
processIf env = 
  case stack env of
    condition:xs -> 
      if condition /= 0
      then env { stack = xs }
      else env { stack = xs, skipDepth = 1 }
    _ -> error "Stack underflow in if"

processEndif :: Env -> Env
processEndif env = env

processElse :: Env -> Env
processElse env = env { skipDepth = 1 }

processDo :: Env -> Env
processDo env = 
  case stack env of
    limit:start:xs -> 
      let frame = LoopFrame
            { startIndex = start
            , endIndex = limit
            , currentIndex = start
            , loopStart = counter env
            , leaveFlag = False
            }
      in env { stack = xs, loopStack = frame : loopStack env }
    _ -> error "Stack underflow in do"

processLoop :: Env -> Env
processLoop env = 
  case loopStack env of
    [] -> error "loop without do"
    (frame:frames) -> 
      if leaveFlag frame
        then env { loopStack = frames }
        else 
          let newIndex = currentIndex frame + 1
          in if newIndex < endIndex frame
             then env { loopStack = (frame { currentIndex = newIndex }) : frames
                      , counter = loopStart frame
                      }
             else env { loopStack = frames }

processI :: Env -> Env
processI env = 
  case loopStack env of
    [] -> error "i without active loop"
    (frame:_) -> env { stack = currentIndex frame : stack env }

processLeave :: Env -> Env
processLeave env = 
  case loopStack env of
    [] -> error "leave without active loop"
    (frame:frames) -> 
      let newFrame = frame { leaveFlag = True }
      in env { loopStack = newFrame : frames }

processDot :: Env -> Env
processDot env = 
  case stack env of
    x:xs -> env { stack = xs, output = output env ++ [show x] }
    _ -> error "Stack underflow in ."

processDotString :: Env -> Env
processDotString env = 
  let remaining = drop (counter env) (ws env)
      (strTokens, consumed) = extractStringTokens remaining
  in if null strTokens && not (any (\s -> "\"" `isSuffixOf` s) remaining)
     then error "Unclosed string"
     else env { counter = counter env + consumed
              , output = output env ++ [intercalate " " strTokens]
              }

extractStringTokens :: [String] -> ([String], Int)
extractStringTokens [] = ([], 0)
extractStringTokens (token:tokens) =
  if "\"" `isSuffixOf` token
    then let cleanToken = takeWhile (/= '"') token
          in ([cleanToken], 1)
    else let (restTokens, restCount) = extractStringTokens tokens
          in (token : restTokens, 1 + restCount)

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf suffix str = suffix == drop (length str - length suffix) str

processCr :: Env -> Env
processCr env = env { output = output env ++ ["\n"] }

processSpace :: Env -> Env
processSpace env = env { output = output env ++ [" "] }

processEmit :: Env -> Env
processEmit env = 
  case stack env of
    x:xs -> env { stack = xs, output = output env ++ [[toEnum x]] }
    _ -> error "Stack underflow in emit"

eva :: String -> [Int] -> ([Int], String)
eva program initialStack = 
    let initial = initialEnv (words program) initialStack
        evaluated = interp initial
    in (stack evaluated, concat (output evaluated))

interp :: Env -> Env
interp env
  | counter env >= length (ws env) = case callStack env of
      [] -> env
      ((retCounter, retWs, retLoopStack, retSkipDepth):rets) -> 
        interp $ env { ws = retWs
                     , counter = retCounter
                     , callStack = rets
                     , loopStack = retLoopStack
                     , skipDepth = retSkipDepth
                     }
  | otherwise = interp $ step env

testStage1 :: IO ()
testStage1 = do
    putStrLn "=== Stage 1 Tests: Calculator ==="
    
    let (t1, _) = eva "1 1 + 1 - 4 * 2 / neg 744 neg *" [1337]
    putStrLn $ "Test 1: " ++ show t1 ++ " == " ++ show (t1 == [1488, 1337])
    
    let (t2, _) = eva "2 3 * 4 5 * +" []
    putStrLn $ "Test 2: " ++ show t2 ++ " == " ++ show (t2 == [26])
    
    let (t3, _) = eva "1 2 3 dup over swap drop" []
    putStrLn $ "Test 3: " ++ show t3 ++ " == " ++ show (t3 == [3, 3, 2, 1])
    
    let (t4, _) = eva "5 3 > 2 4 < and" []
    putStrLn $ "Test 4: " ++ show t4 ++ " == " ++ show (t4 == [-1])
    
    let (t5, _) = eva "10 3 mod" []
    putStrLn $ "Test 5: " ++ show t5 ++ " == " ++ show (t5 == [1])
    
    let (t6, _) = eva "5 neg" []
    putStrLn $ "Test 6: " ++ show t6 ++ " == " ++ show (t6 == [-5])

testStage2 :: IO ()
testStage2 = do
    putStrLn "\n=== Stage 2 Tests: Word Definitions ==="
    
    let (t1, _) = eva " define inc 1 + end 5 inc " []
    putStrLn $ "Test 1: " ++ show t1 ++ " == " ++ show (t1 == [6])
    
    let (t2, _) = eva " define square dup * end 5 square " []
    putStrLn $ "Test 2: " ++ show t2 ++ " == " ++ show (t2 == [25])
    
    let (t3, _) = eva " define double 2 * end define quadruple double double end 3 quadruple " []
    putStrLn $ "Test 3: " ++ show t3 ++ " == " ++ show (t3 == [12])
    
    let (t4, _) = eva " define add3 + + end 1 2 3 add3 " []
    putStrLn $ "Test 4: " ++ show t4 ++ " == " ++ show (t4 == [6])
    
    let (t5, _) = eva " define inc 1 1 + * end 3 inc " []
    putStrLn $ "Test 5: " ++ show t5 ++ " == " ++ show (t5 == [6])

testStage3 :: IO ()
testStage3 = do
    putStrLn "\n=== Stage 3 Tests: Control Structures ==="
    
    let (t1, _) = eva " define abs                    \
                 \   dup 0 <                    \
                 \   if neg                     \
                 \   endif                      \
                 \ end                          \
                 \ 5 abs                        \
                 \ -5 abs                       " []
    putStrLn $ "Test 1 (absolute value): " ++ show t1 ++ " == " ++ show (t1 == [5,5])
    
    let (t2, _) = eva " 5 3 >                       \
                 \ if 100                      \
                 \ else 200                    \
                 \ endif                       " []
    putStrLn $ "Test 2 (if true): " ++ show t2 ++ " == " ++ show (t2 == [100])
    
    let (t3, _) = eva " 0                           \
                 \ if 100                      \
                 \ else 200                    \
                 \ endif                       " []
    putStrLn $ "Test 3 (if false): " ++ show t3 ++ " == " ++ show (t3 == [200])
    
    let (t4, _) = eva " define max                  \
                 \   over over <               \
                 \   if swap drop              \
                 \   else drop                 \
                 \   endif                     \
                 \ end                         \
                 \ 5 10 max                    \
                 \ 10 5 max                    " []
    putStrLn $ "Test 4 (max): " ++ show t4 ++ " == " ++ show (t4 == [10,10])
    
    let (t5, _) = eva " define fact                 \
                 \   1 swap 1 + 1 swap         \
                 \   1 swap do i * loop        \
                 \   swap drop                 \
                 \ end                         \
                 \ 5 fact                      " []
    putStrLn $ "Test 5 (factorial 5): " ++ show t5 ++ " == " ++ show (t5 == [120])
    
    let (t6, _) = eva " define sum                  \
                 \   0 1 11 do i + loop        \
                 \ end                         \
                 \ sum                         " []
    putStrLn $ "Test 6 (sum 1..10): " ++ show t6 ++ " == " ++ show (t6 == [55])

    let (t7, _) = eva " define leave-test           \
                 \   0 1 11 do                 \
                 \     i 5 >                   \
                 \     if leave 1 +            \
                 \     else i +                \
                 \     endif                   \
                 \   loop                      \
                 \ end                         \
                 \ leave-test                  " []
    putStrLn $ "Test 7 (leave): " ++ show t7 ++ " == " ++ show (t7 == [16])
    
    
    let (t8, _) = eva " 0 1 11 do i + loop         " []
    putStrLn $ "Test 8 (sum 1..10 without define): " ++ show t8 ++ " == " ++ show (t8 == [55])
    
    let (t9, _) = eva " define sign                 \
                 \   dup 0 >                   \
                 \   if drop 1                 \
                 \   else                      \
                 \     dup 0 <                 \
                 \     if drop -1              \
                 \     else drop 0             \
                 \     endif                   \
                 \   endif                     \
                 \ end                         \
                 \ 5 sign                      \
                 \ -3 sign                     \
                 \ 0 sign                      " []
    putStrLn $ "Test 9 (sign function): " ++ show t9 ++ " == " ++ show (t9 == [0, -1, 1])
    
    let (t10, _) = eva " define fib                     \
                  \   dup 0 =                      \
                  \   if                           \
                  \   else                         \
                  \     0 swap 1 swap              \
                  \     1 swap do swap over + loop \
                  \     swap drop                  \
                  \   endif                        \
                  \ end                            \
                  \ 8 fib                     " []
    putStrLn $ "Test 10 (fibonacci 8): " ++ show t10 ++ " == " ++ show (t10 == [21])


testStage4 :: IO ()
testStage4 = do
    putStrLn "\n=== Stage 4 Tests: Output ==="
    
    let (stack1, output1) = eva "1 . 2 . 3 ." []
    putStrLn $ "Test 1: output='" ++ output1 ++ "' == '123' " ++ 
               show (output1 == "123") ++ " && stack=" ++ show stack1 ++ 
               " == [] " ++ show (stack1 == [])
    
    let (stack2, output2) = eva ".\" Hello\" " []
    putStrLn $ "Test 2: output='" ++ output2 ++ "' == 'Hello' " ++ 
               show (output2 == "Hello") ++ " && stack=" ++ show stack2 ++ 
               " == [] " ++ show (stack2 == [])
    
    let (stack3, output3) = eva "65 emit 66 emit" []
    putStrLn $ "Test 3: output='" ++ output3 ++ "' == 'AB' " ++ 
               show (output3 == "AB") ++ " && stack=" ++ show stack3 ++ 
               " == [] " ++ show (stack3 == [])
    
    let (stack4, output4) = eva "1 . space 2 ." []
    putStrLn $ "Test 4: output='" ++ output4 ++ "' == '1 2' " ++ 
               show (output4 == "1 2") ++ " && stack=" ++ show stack4 ++ 
               " == [] " ++ show (stack4 == [])
    
    let (stack5, output5) = eva "1 2 3 . . ." []
    putStrLn $ "Test 5: output='" ++ output5 ++ "' == '321' " ++ 
               show (output5 == "321") ++ " && stack=" ++ show stack5 ++ 
               " == [] " ++ show (stack5 == [])
    
    let (stack6, output6) = eva ".\" Hello\" space .\" World\" " []
    putStrLn $ "Test 6: output='" ++ output6 ++ "' == 'Hello World' " ++ 
               show (output6 == "Hello World") ++ " && stack=" ++ show stack6 ++ 
               " == [] " ++ show (stack6 == [])
    
    let (stack7, output7) = eva "1 . cr 2 ." []
    putStrLn $ "Test 7: output='" ++ output7 ++ "' == '1\\n2' " ++ 
               show (output7 == "1\n2") ++ " && stack=" ++ show stack7 ++ 
               " == [] " ++ show (stack7 == [])
    
    let (stack8, output8) = eva ".\" Hello World \" " []
    putStrLn $ "Test 8: output='" ++ output8 ++ "' == 'Hello World' " ++ 
               show (output8 == "Hello World") ++ " && stack=" ++ show stack8 ++ 
               " == [] " ++ show (stack8 == [])
    
    let (stack9, output9) = eva ".\" Test\" cr .\" Multiple\" cr .\" Lines\" " []
    putStrLn $ "Test 9: output='" ++ output9 ++ "' == 'Test\\nMultiple\\nLines' " ++ 
               show (output9 == "Test\nMultiple\nLines") ++ " && stack=" ++ show stack9 ++ 
               " == [] " ++ show (stack9 == [])
    
    let (stack10, output10) = eva "1 . space 2 . space 3 ." []
    putStrLn $ "Test 10: output='" ++ output10 ++ "' == '1 2 3' " ++ 
               show (output10 == "1 2 3") ++ " && stack=" ++ show stack10 ++ 
               " == [] " ++ show (stack10 == [])

main :: IO ()
main = do
    putStrLn "Starting Forth Interpreter Tests"
    
    testStage1
    testStage2
    testStage3
    testStage4
```

# Вывод
…пишете, чему научились…
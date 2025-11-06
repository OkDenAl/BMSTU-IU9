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
                       Add e1 e2 -> showExpr currentPrec False e1 ++ " + " ++ showExpr currentPrec True e2
                       Sub e1 e2 -> showExpr currentPrec False e1 ++ \
                       " - " ++ showExpr currentPrec True e2
                       Mul e1 e2 -> showExpr currentPrec False e1 ++  \
                       " * " ++ showExpr currentPrec True e2
                       Div e1 e2 -> showExpr currentPrec False e1 ++ \
                        " / " ++ showExpr currentPrec True e2
                       Pow e1 e2 -> showExpr currentPrec False e1 ++\
                        " ^ " ++ showExpr currentPrec True e2
                       Number n  -> show n
                       Var name  -> name
            in if needParens then "(" ++ str ++ ")" else str

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


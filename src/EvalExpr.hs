--
-- EPITECH PROJECT, 2021
-- Untitled (Workspace)
-- File description:
-- EvalExpr
--

module EvalExpr where

import Control.Applicative
import Parse

-- add ::= mul + add <|> mul - add <|> mul
-- mul ::= pow * mul <|> pow / mul <|> pow
-- pow ::= num ^ pow <|> num
-- num ::= dig <|> (add)
-- dig ::= [0..9]

data PAR = PRIO ADD
        | DIG Float
        deriving (Show)

data EXP = POW PAR EXP
        | PSOLO PAR
        deriving (Show)

data MUL = MULPOW EXP MUL
        | DIVPOW EXP MUL
        | MSOLO EXP
        deriving (Show)

data ADD = ADDOP MUL ADD
        | SUBOP MUL ADD
        | ASOLO MUL
        deriving (Show)

data AST = Add ADD | Mul MUL | Exp EXP | Par PAR

eval :: AST -> Float
eval (Add (ADDOP i j)) = eval (Mul i) + eval (Add j)
eval (Add (SUBOP i j)) = eval (Mul i) - eval (Add j)
eval (Add (ASOLO i)) = eval (Mul i)
eval (Mul (MULPOW i j)) = eval (Exp i) * eval (Mul j)
eval (Mul (DIVPOW i j)) = eval (Exp i) / eval (Mul j)
eval (Mul (MSOLO i)) = eval (Exp i)
eval (Exp (POW i j)) = eval (Par i) ** eval (Exp j)
eval (Exp (PSOLO i)) = eval (Par i)
eval (Par (PRIO i)) = eval (Add i)
eval (Par (DIG i)) = i



--        Add (SUBOP i j) -> MUL i - ADD j
--        Mul (MULPOW i j) -> EXP i * MUL j
--        Mul (DIVPOW i j) -> EXP i / MUL j
--        Exp (POW i j) -> PAR i ^ POW j
--        Par (PRIO i) -> ADD i
--        Par (DIG i) -> i

--add :: Parser AST
--add = setExpr ADDOP mul add '+' <|> setExpr SUBOP mul add '-' <|> mul

parseAst :: Parser AST
parseAst = Add <$> add

add :: Parser ADD
add = (ADDOP <$> mul <*> (parseChar '+' *> add)) <|> (SUBOP <$> mul <*> (parseChar '-' *> add)) <|> ASOLO <$> mul

mul :: Parser MUL
mul = (MULPOW <$> pow <*> (parseChar '*' *> mul)) <|> (DIVPOW <$> pow <*> (parseChar '/' *> mul)) <|> MSOLO <$> pow

pow :: Parser EXP
pow = (POW <$> par <*> (parseChar '^' *> pow)) <|> PSOLO <$> par

par :: Parser PAR
par = DIG <$> parseSpace parseFloat <|> PRIO <$> parseSpace (parseChar '(' *> add <* parseChar ')')

-- num :: Parser Float
-- num = Parser func where
--     func str = case runParser (parseMany (parseChar ' ')) str of
--         Nothing -> Nothing
--         Just (a, string) -> Just (read string :: Float, a)

parseSpace :: Parser a -> Parser a
parseSpace = func where
        func str = parseMany (parseChar ' ') *> str <* parseMany (parseChar ' ')

--setExpr :: AST -> Parser AST -> Parser AST -> Char -> Parser AST
--setExpr expr a b op = expr <$> a <*> (parseMany (parseChar ' ') *> parseChar op ) *> b
--faire ParserOperator

evalExpr :: String -> Float
evalExpr str = case runParser parseAst str of
                Just (a, b) -> eval a
                Nothing -> 0

-- data EXPR = ADD EXPR EXPR
--         | SUB EXPR EXPR
--         | MUL EXPR EXPR
--         | DIV EXPR EXPR
--         | POW EXPR EXPR
--         | DIG Float  
--         deriving Show

-- --test = MUL (ADD (DIG 20) (DIG 10)) (ADD (DIG 20) (DIG 10))

-- -- (20 + 10) * (20 + 10) 

-- eval :: EXPR -> Float
-- eval x = case x of
--     ADD i j -> eval i + eval j
--     SUB i j -> eval i - eval j
--     MUL i j -> eval i * eval j
--     DIV i j -> eval i / eval j
--     POW i j -> eval i ^ eval j
--     DIG i -> i 

-- add :: Parser Float
-- add = parseOr mul (parseOr (setExpr ADD mul add '+') (setExpr SUB mul add '-'))

-- mul :: Parser Float
-- mul = parseOr pow (parseOr (setExpr MUL pow mul '*') (setExpr DIV pow mul '/'))

-- pow :: Parser Float
-- pow = parseOr num (setExpr POW num pow '^')

-- num :: Parser Float 
-- num = parseOr (fmap (read ::Float) space) (parsechar ')' add parseChar '(')

-- space :: Parser String 
-- space = parseMany (parseChar ' ')

-- setExpr :: EXPR -> Parser EXPR -> Parser EXPR -> Char -> Parser Float
-- setExpr exp a b op = case result of
--         Just(x, y) -> Parser pfloat
--         Nothing-> Parser pNothing
--         where
--             pNothing = Nothing 
--             pfloat str = Just (eval exp, str)
--             result = case runParser a of
--                 Just(x, xr) -> case runParser (b (parseChar op)) of
--                     Just(y, yr) -> Just(x, y)
--                     Nothing -> Nothing
--                 Nothing -> Nothing


-- expr :: Parser Float 
-- expr = add

-- evalExpr :: String -> Float 
-- evalExpr x = res
--         where Just (res, _) = runParser expr x
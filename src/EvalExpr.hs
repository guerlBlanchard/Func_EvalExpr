--
-- EPITECH PROJECT, 2021
-- Untitled (Workspace)
-- File description:
-- EvalExpr
--

module EvalExpr
    (
    )where

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
        deriving (Show)

data MUL = MULPOW EXP MUL
        | DIVPOW EXP MUL
        deriving (Show)

data ADD = ADDOP MUL ADD
        | SUBOP MUL ADD
        deriving (Show)

eval :: ADD -> Float
eval x = case x of
        ADDOP i j -> MUL i + ADD j
        SUBOP i j -> MUL i - ADD j
        MULPOW i j -> EXP i * MUL j
        DIVPOW i j -> EXP i / MUL j
        POW i j -> PAR i ^ POW j
        PRIO i -> ADD i
        DIG i -> i

add :: Parser ADD
add = mul + add <|> mul - add <|> mul

mul :: Parser MUL
mul = pow * mul <|> pow / mul <|> pow

pow :: Parser EXP
pow = par ^ pow <|> par

par :: Parser PAR
par = num <|> parseChar '(' *> add <* parseChar ')'

num :: Parser Float
num = Parser func where
    func str = case runParser (parseMany (parseChar ' ')) str of
        Nothing -> Nothing
        Just (a, string) -> Just (read a:: Float, string)

space :: Parser a
space = runParser (parseMany (parseChar ' '))

setExpr :: ADD -> Parser ADD -> Parser ADD -> Char -> Parser ADD
setExpr expr a b op x = runParser (expr <$> a <*> (parseMany (parseChar ' ') *> parseChar op ) *> b) x

evalExpr :: String -> Float
evalExpr = fmap eval runParser add

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
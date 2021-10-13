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

data ADD = ADDOP MUL ADD
        | SUBOP MUL ADD

data MUL = MULPOW EXP MUL
        | DIVPOW EXP MUL

data EXP = POW NUM EXP

data PAR = PRIO ADD
        | CALC NUM

data NUM = DIG Float

eval :: ADD -> Float
eval x = case x of
        ADDOP i j -> MUL i + ADD j
        SUBOP i j -> MUL i - ADD j
        MULPOW i j -> EXP i * MUL j
        DIVPOW i j -> EXP i / MUL j
        POW i j -> PAR i ^ POW j
        PRIO i -> ADD i
        CALC i -> NUM i
        DIG i -> i

add :: Parser ADD
add = parseOr (mul + add) (parseOr (mul - add) mul)

mul :: Parser MUL
mul = parseOr (pow * mul) (parseOr (pow / mul) pow)

pow :: Parser EXP
pow = parseOr (par) (par ^ pow)

par :: Parser PAR
par = parseOr (add) (num)

num :: Parser Float
num = (fmap read ::Float) parseMany (parseChar ' ')

evalExpr :: String -> Float
evalExpr = fmap eval runParser add











-- data EXPR = ADD EXPR EXPR
--         | SUB EXPR EXPR
--         | MUL EXPR EXPR
--         | DIV EXPR EXPR
--         | POW EXPR EXPR
--         | DIG Int 

-- -- test = MUL (ADD (DIG 20) (DIG 10)) (ADD (DIG 20) (DIG 10))

-- -- 20 + 10 * 20 + 10 == (20 + 10) * (20 + 10)

-- eval :: EXPR -> Int 
-- eval x = case x of
--     ADD i j -> eval i + eval j
--     SUB i j -> eval i - eval j
--     MUL i j -> eval i * eval j
--     DIV i j -> eval i / eval j
--     POW i j -> eval i ^ eval j
--     DIG i -> i 

-- add :: Parser EXPR
-- add = parseOr mul (parseOr (mul + add) (mul - add))

-- mul :: Parser EXPR
-- mul = parseOr pow (parseOr (pow * mul) (pow / mul))

-- pow :: Parser EXPR
-- pow = parseOr num (num ^ pow)

-- num :: Parser EXPR
-- num = parseOr parseInt add

-- delop :: Parser EXPR -> Char -> Parser EXPR
-- delop next op = next parseAnd (parseMany (parseChar ' ')) parseChar op

-- expr :: Parser EXPR 
-- expr = add

-- evalExpr :: String -> Int
-- evalExpr x = eval res
--         where Just (res, _) = runParser expr x
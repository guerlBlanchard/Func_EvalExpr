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

data EXPR = ADD EXPR EXPR
        | SUB EXPR EXPR
        | MUL EXPR EXPR
        | DIV EXPR EXPR
        | POW EXPR EXPR
        | DIG Int 

expr :: EXPR -> Int 
expr x = case x of
    ADD i j -> expr i + expr j
    SUB i j -> expr i - expr j
    MUL i j -> expr i * expr j
    DIV i j -> expr i / expr j
    POW i j -> expr i ^ expr j
    DIG i -> i

eval :: Parser EXPR 
eval = add
    where
        add = parseOr mul (parseOr (mul + add) (mul - add))
        mul = parseOr pow (parseOr (pow * pow) (pow / pow))
        pow = parseOr num (num ^ pow)
        num = parseOr (DIG) (add)

evalExpr :: String -> Int
evalExpr = fmap expr $ runParser eval
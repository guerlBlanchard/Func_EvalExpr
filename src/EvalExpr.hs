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

evalExpr :: String -> Maybe Int
evalExpr = add
    where   add = mult + mult parseOr mult - mult parseOr mult
            mult = opt * opt parseOr opt / opt parseOr opt
            opt = num parseOr evalExpr
            num = parseInt
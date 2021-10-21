--
-- EPITECH PROJECT, 2021
-- Untitled (Workspace)
-- File description:
-- EvalExpr
--

module EvalExpr where

import Control.Applicative
import Parse
import System.Exit (exitWith, ExitCode (ExitFailure), exitSuccess, exitFailure)
import GHC.Real (infinity)
import qualified GHC.Real as Float
import Numeric

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

printResult :: (Float, String) -> IO ()
printResult (a, "") = if isInfinite a
                        then exitWith(ExitFailure 84)
                        else print $ showFFloat (Just 2) a ""
printResult (_, _) = exitWith(ExitFailure 84)

evalExpr :: String -> IO ()
evalExpr str = case runParser parseAst str of
                Just (a, b) -> printResult (eval a, b)
                Nothing -> exitWith(ExitFailure 84)
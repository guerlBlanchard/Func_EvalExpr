--
-- EPITECH PROJECT, 2021
-- Untitled (Workspace)
-- File description:
-- Parse
--

module Parse
    (
    parseChar,
    parseAnyChar,
    parseAnd,
    parseAndWith,
    parseOr,
    parseMany,
    parseSome,
    parseUInt,
    parseInt
    )where

import Text.Read

type Parser a = String -> Maybe (a, String)

parseTuple :: Parser a -> Parser (a,a)
parseTuple a x = case parseChar '(' x of
    Just (_, par1) -> case a par1 of
        Just (r1, x1) -> case parseChar ',' x1 of
            Just (_, vir) -> case a vir of
                Just (r2, par2) -> case parseChar ')' par2 of
                    Just (_, rest) -> Just ((r1, r2), rest)
                    Nothing -> Nothing
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing

parseInt :: Parser Int
parseInt x = case parseChar '-' x of
    Just (_, xr1) -> case parseUInt xr1 of
        Just (r, xr2) -> Just (-r, xr2)
        Nothing -> Nothing
    Nothing -> parseUInt x

parseUInt :: Parser Int
parseUInt x = case parseSome (parseAnyChar ['0'..'9']) x of
    Just (r, xr) -> Just (read r :: Int, xr)
    Nothing -> Nothing

parseSome :: Parser a -> Parser [a]
parseSome a (x:xs) = case a (x:xs) of
    Just (r, xr) -> Just ([r] ++ jr, jx)
        where Just (jr, jx) = parseMany a xs
    Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany a (x:xs) = case a (x:xs) of
    Just (r, xr) -> Just ([r] ++ jr, jx)
        where Just (jr, jx) = parseMany a xs
    Nothing -> Just ([], (x:xs))

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith function a b x = case a x of
    Just (r1, xs) -> case b xs of
        Just (r2, xr) -> Just (function r1 r2, xr)
        Nothing -> Nothing
    Nothing -> Nothing

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd a b x = case a x of
    Just (r1, xs) -> case b xs of
        Just (r2, xr) -> Just ((r1, r2), xr)
        Nothing -> Nothing
    Nothing -> Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr a b x = case a x of
    Just (r, xr) -> Just (r, xr)
    Nothing -> b x

parseAnyChar :: String -> Parser Char
parseAnyChar _ "" = Nothing
parseAnyChar "" b = Nothing
parseAnyChar a (b:bs)
        | elem b a = Just(b, bs)
        | otherwise = Nothing 

parseChar :: Char -> Parser Char
parseChar _ [] = Nothing
parseChar a (x:xs)  | a == x = Just (x, xs)
                    | otherwise = Nothing
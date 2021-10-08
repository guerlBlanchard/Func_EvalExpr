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
    parseInt,
    parseTuple
    )where

import Text.Read

data Parser a = Parser {
    runParser :: String -> Maybe (a, String) 
}

parseChar :: Char -> Parser Char
parseChar a = Parser func where
              func [] = Nothing
              func (x:xs)
                    | a == x = Just (x, xs)
                    | otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar a = Parser func where
                    func [] = Nothing
                    func (b:bs)
                        | b `elem` a = Just(b, bs)
                        | otherwise = Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr (Parser b) (Parser c) = Parser func where
                func x = case b x of
                    Just (r, xr) -> Just (r, xr)
                    Nothing -> c x

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd (Parser a) (Parser b) = Parser func where
                func x = case a x of
                    Just (r1, xs) -> case b xs of
                        Just (r2, xr) -> Just ((r1, r2), xr)
                        Nothing -> Nothing
                    Nothing -> Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith function (Parser a) (Parser b) = Parser func where 
                func x = case a x of
                    Just (r1, xs) -> case b xs of
                        Just (r2, xr) -> Just (function r1 r2, xr)
                        Nothing -> Nothing
                    Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany (Parser a) = Parser func where
                    func [] = Just ([], [])
                    func (x:xs) = case a (x:xs) of
                        Just (r, xr) -> Just ([r] ++ jr, jx)
                            where Just (jr, jx) = runParser (parseMany (Parser a)) xs
                        Nothing -> Just ([], (x:xs))

parseSome :: Parser a -> Parser [a]
parseSome (Parser a) = Parser func where
                    func (x:xs) = case a (x:xs) of
                        Just (r, xr) -> Just ([r] ++ jr, jx)
                            where Just (jr, jx) = runParser (parseMany (Parser a)) xs
                        Nothing -> Nothing


parseUInt :: Parser Int
parseUInt = Parser func where
                    func x = read fmap runParser (parseSome (parseAnyChar ['0'..'9'])) x

parseInt :: Parser Int
parseInt = Parser func where
                    func x = case runParser (parseChar '-') x of
                        Just (_, xr1) -> case runParser parseUInt xr1 of
                            Just (r, xr2) -> Just (-r, xr2)
                            Nothing -> Nothing
                        Nothing -> runParser parseUInt x

parseTuple :: Parser a -> Parser (a,a)
parseTuple (Parser a) = Parser func where
                    func x = case runParser (parseChar '(') x of
                        Just (_, par1) -> case a par1 of
                            Just (r1, x1) -> case runParser (parseChar ',') x1 of
                                Just (_, vir) -> case a vir of
                                    Just (r2, par2) -> case runParser (parseChar ')') par2 of
                                        Just (_, rest) -> Just ((r1, r2), rest)
                                        Nothing -> Nothing
                                    Nothing -> Nothing
                                Nothing -> Nothing
                            Nothing -> Nothing
                        Nothing -> Nothing
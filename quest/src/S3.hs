{-# LANGUAGE BlockArguments #-}

module S3 where

import Data.Char (Char, isDigit)
import Data.Maybe (fromJust)

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
  deriving (Eq, Show)

--iid w@(x:xs)|isDigit x=iid xs
--          |otherwise =
--iid []=Just (Number
--instance Monad Token where
--return "+"=Just Plus
--return "-"=Just Minus
--return "("=Just LeftBrace
--return ")"=Just RightBrace
--return  x= Just (read x::Int)
--fail =Nothing
iid w@(x : xs)
  | isDigit x = iid xs
  | otherwise = False
iid [] = True

asToken :: String -> Maybe Token
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken w@(x : xs)
  | isDigit x = Just (Number (read w :: Int))
  | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize input =foldr f (return []) (words input) where
	f word list = do
		token <- asToken word
		tokens <- list
		return $ token : tokens
--tokenize input = inp [asToken [x]|x<-input,x/=' '] where
--                                      inp (x:[])=Just [fromJust x]
--                                      inp w@(x:xs)| ( filter( \x->x==Nothing) w)==[]=Just (fromJust x:fromJust(inp xs))
--                                                  |  otherwise=Nothing

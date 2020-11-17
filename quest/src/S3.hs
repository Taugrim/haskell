{-# LANGUAGE BlockArguments #-}

module S3 where

import Text.Printf
import Data.Char (Char, isDigit)
import Data.Maybe (fromJust)
import Data.List
import System.Directory
import Control.Monad
import Control.Monad.Reader
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
--ss::IO Char->IO Char
ss ch|ch==' '= error "sfsf"

ss ch= ch
st=do
    putStrLn "What is your name?"
    putStr "Name: "
    nm<-getChar
    if nm=='\n' then
      st
      else do
      s<-getLine
      return (nm:s)


m =do

   c<-st
   putStrLn $ "Hi, "++c++"!"

delF= do
         putStr "Substring: "
         nm<-getChar
         if nm=='\n' then
               putStrLn "Canceled"
               else do
                      s<-getLine
                      lf<- fmap (fmap (filter (isInfixOf s))) getDirectoryContents"."
                      mapM_ (\x -> putStrLn ("Removing file: " ++ x) >> removeFile x)  lf

--filter (isInfixOf "V1") (getDirectoryContents getCurrentDirectory)прсш
--getDirectoryContents "/home/q/git/haskell/quest/src"
--
--filt a= filter (isInfixOf a) ["S1.hs",".","Stepic.hs","..","S3.hs","Quest.hs","S2.hs"]
--filt a= filter (isInfixOf "S1") ["S1.hs",".","Stepic.hs","..","S3.hs","Quest.hs","S2.hs"]
data Reader' r a = Reader' { runReader' :: (r -> a) }

instance Monad (Reader' r) where
  return x = Reader' $ \_ -> x
  m >>= k  = Reader' $ \r -> runReader' (k (runReader' m r)) r
local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m =  Reader $ \e ->runReader m (f e)
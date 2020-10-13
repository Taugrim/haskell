--{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Char(isDigit, Char)
import Data.Maybe
import Prelude ((.))
import Prelude hiding (return,(>>=))
findDigit :: [Char] -> Maybe Char
findDigit w@(x:xs)= fd False w ' ' where
                                  fd::Bool->[Char]->Char->Maybe Char
                                  fd False (w:ws) _=fd (isDigit w) ws w
                                  fd True (w:ws) ww=Just ww
                                  fd _ _ _=Nothing
findDigit' :: [Char] -> Maybe Char
findDigit' w@(x:xs)= fd [q|q<-w,isDigit q] where
                                             fd (x:_)=Just x
                                             fd _=Nothing



--findDigit :: [Char] -> Maybe Char

findDigitOrX :: [Char] -> Char
findDigitOrX w =
  case findDigit w of
        Just a-> a
        _->'X'
maybeToList :: Maybe a -> [a]
maybeToList  Nothing=[]
maybeToList  (Just a)=[a]

listToMaybe :: [a] -> Maybe a
listToMaybe []= Nothing
listToMaybe d= Just (head d)
ss="firstName = John\nlastName = Connor\nage = 30"

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show
data Person = Person { firstName :: String, lastName :: String, age :: Int }deriving Show
--customLines::String->Maybe [String]

parsePerson :: String -> Either Error Person
parsePerson str= l  where
                    w=lines str
                    ww= map ff w 
                    ff s=[af s,"=",bf s]
                    af('=':xs)=xs
                    af(x:xs)=af xs
                    af[]=[]
                        
                    bf ('=':xs)=[]
                    bf (x:xs)=x:bf xs
                    bf []=[]
                    fFormat = (Prelude.filter fl ww)/=[]
                    fl (q:qq:qqq:[])=qq/="="
                    fl _=True

                    filterFilds ::String->[String]->[String]
                    filterFilds str w=filter (==str) w

                    filterFilds1 ::String->[[String]]->String
                    filterFilds1 str w= head $head (filter (\ q->(filterFilds str q)/=[]) w)
                    fAge (x:xx:[age,"=","age"]:[])= age
                    fAge _= ""
                    iid (x:xs)|isDigit x=iid xs
                              |otherwise =False
                    iid []=True
                    fa=fAge ww
                    toI x=read x ::Int
                    l:: Either Error Person
                    l   |length w<3= Left IncompleteDataError
                        |fFormat = Left ParsingError
                        |otherwise =
                            if not $ iid fa
                            then Left (IncorrectDataError fa)
                            else  Right Person { firstName=filterFilds1 "firstName" ww 
                            , lastName =filterFilds1 "lastName" ww
                            , age = toI (filterFilds1 "age" ww)}

w =lines
ww s= map ff (w s)
ff s=[af s,"=",bf s]
af('=':xs)=xs
af(x:xs)=af xs
af[]=[]

bf ('=':xs)=[]
bf (x:xs)=x:bf xs
bf []=[]
fFormat s= (Prelude.filter fl (ww s))/=[]
fl (q:qq:qqq)=qq/="="
fl _=False
flf (q:qq:qqq)=qq

filterFilds ::String->[String]->[String]
filterFilds str w=filter (==str) w

filterFilds1 ::String->[[String]]->[[String]]
filterFilds1 str w= filter (\ q->(filterFilds str q)/=[]) w

--customLines :: String -> Either Error [String]
--customLines str=l w w where
--                    w []= Nothing
--                    w s= Just (words s)
--                    l Nothing _=ParsingError
--                    l s ss|Prelude.length s\=3 =ParsingError
--
--                    l ss@(x:xx:xs:[])=Just ss
--                    l ss@("":_:_:_)=Nothing
--                    l ss@(_:"":_:_)=Nothing
--                    l ss@(_:_:"":_)=Nothing
--                    l _=Nothing

--customWords :: Maybe [String] -> Either Error Person
--customWords Nothing= Left ParsingError

--customWords (Just arr)= w (map  words arr) where w arr=

--d s=map  words (lines s)
eitherToMaybe :: Either a a-> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing

data Coord a = Coord a !a 
getX :: Coord a -> a
getX (Coord x _) = x

getY :: Coord a -> a
getY (Coord _ y) = y


data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
    fmap f (Point3D x y z) = Point3D (f x)(f y)(f z)
data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)deriving Show
instance Functor GeomPrimitive where
    fmap f (Point a)=Point (fmap f a)
    fmap f (LineSegment a b)=LineSegment (fmap f a) (fmap f b )
    


data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show
instance Functor Tree where
    fmap f (Leaf a) =Leaf (fmap f a)
    fmap f (Branch a b c) =Branch  (fmap f a)(fmap f b)(fmap f c)




--instance Functor ((,) a) where
--    fmap f (x,y) =(x,f y)
--data Entry k1 k2= Entry (k1, k2)   deriving Show
data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show
instance Functor (Entry k1 k2) where
    fmap f (Entry k1 k2 ) = Entry k1 (f k2)
instance Functor (Map k1 k2) where
    fmap f (Map x )= Map (map (fmap f ) x)


data Log a = Log [String] a deriving Show
instance Functor (Log) where
    fmap f (Log k1 k2 ) = Log k1 (f k2)
instance Applicative Log where
pure = Log
(<*>)  f = fmap f
toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f str=fmap (Log [str]) f 
add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f1 f2= l3 l1 (l2 l1) where 
                                l1= f1 x
                                l2 (Log s a)=f2 a
                                l3 (Log s a)(Log s1 a1)=Log (s++s1) a1
returnLog :: a -> Log a
returnLog = Log []  

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog  (Log s a) f=  Log (s++s1) b where  Log s1 b= f a     


instance Monad Log where 
return=returnLog
(>>=)=bindLog


execLoggersList :: a -> [a -> Log a] -> Log a             
execLoggersList  a fa=foldl ((>>=)) (return a)  fa
--data SomeType a = undefined
--instance Functor SomeType where
--    fmap f x =x (>>=)\y->return(f y)(>>=)
   
data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)
    
iid w@(x:xs)|isDigit x=iid xs
          |otherwise =False
iid []=True
instance Monad  Token where
return "+"=Just Plus
--return "-"=Just Minus
--return "("=Just LeftBrace
--return ")"=Just RightBrace
--return  x= Just (read x::Int)
fail =Nothing
asToken :: String -> Maybe Token
asToken =return
import Data.Char(isDigit, Char)
import Data.Maybe
import Prelude
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
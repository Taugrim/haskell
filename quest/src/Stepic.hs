{-# LANGUAGE FlexibleContexts #-}

module Stepic where


data LogLevel = Error | Warning | Info 

cmp::LogLevel->LogLevel->Ordering
cmp Error Error=EQ
cmp Error _=GT
cmp Warning Warning=EQ
cmp Warning Error=LT
cmp Warning Info=GT
cmp Info Info=EQ
cmp Info _=LT

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x y) (Point x1 y1) = distanceToOrigin (Point (x1-x) (y1-y))
data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r)= pi*r*r
area (Rectangle q w)= q*w

data Result = Fail | Success
data SomeData = SD|DS
doSomeWork :: SomeData -> (Result,Int)
doSomeWork SD=(Success,0)
doSomeWork DS=(Fail,1)

processData::SomeData->String
processData =str.doSomeWork  where
--                        str::(Result,Int)->String
                        str (Success,_)="Success"
                        str (_,y)="Fail: "++show y
                        
data Result' = F Result Int| S Result

instance Show Result' where
    show (S Success)= "Success"
    show (F Fail y)= "Fail: "++show y

doSomeWork' :: SomeData -> Result'
doSomeWork' n=case doSomeWork n of 
                      (Fail,x)->F Fail x
                      (Success,_)-> S Success

--data Shape = Circle Double | Rectangle Double Double

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle x y)= x==y
isSquare _= False


data Bit = Zero | One deriving Show
data Sign = Minus | Plus deriving Show
data Z = Z Sign [Bit] deriving Show

add :: Z -> Z -> Z
add (Z s (x:xs))(Z s1 (x1:xs1))= add12 s (x:xs) s1(x1:xs1) where

                                            add12 ss (xx:xxs) ss1(xx1:xxs1)=Z ss (add1(xx:xxs)(xx1:xxs1) [])
                                            add1(xx:xxs)(xx1:xxs1) b=y:add1 xxs xxs1 b 
                                            add1(xx:xxs) _ b=y:add1 xxs [Zero] b 
                                            add1 _ (xx1:xxs1) b=y:add1 [Zero] xxs1 b where 
                                            add1 _ _ b=b
                                            pl Zero Zero=(Zero,Zero)
                                            pl One One=(One,One)
                                            pl _ _=(One,Zero)
                                            (y,i)=pl x x1
                                            
add _ _= error"ssdsdd"

mul :: Z -> Z -> Z
mul = undefined
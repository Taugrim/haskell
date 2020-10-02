module Quest where

import Prelude

myLast :: [a] -> a
myLast (x : []) = x
myLast (x : xs) = myLast xs
myLast [] = error "error"

myLast1 = head . reverse

myButLast = head . tail . reverse

elementAT x 0 = head x
elementAT (x : xs) n = elementAT xs (n -1)
elementAT _ _ = error "jkhkjh"

myLength [] = 0
myLength a = foldr (\_ -> (1 +)) 0 a

-- myReverse []=[]

myReverse :: [a] -> [a]
myReverse = foldl (\ys x -> x : ys) []

ispalindrome :: (Eq a) => [a] -> Bool
ispalindrome a = a == (reverse a)

data NestList a = Elem a | List [NestList a]

flatten :: NestList a -> [a]
flatten (Elem a) = [a]
flatten (List b) = foldr (++) [] $map flatten b

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x : []) = [x]
compress (x : x1 : xs)
  | x == x1 = compress (x : xs)
  | otherwise = x : compress (x1 : xs)

-- pak::(Eq a)=> [a]->[[a]]
-- pak []=[[]]
-- pak (x:[])=[[x]]
-- pak (x:x1:xs)|x==x1=foldr (x:) [] compress (x1:xs)
--                   |otherwise=[x]++compress (x1:xs)

-- li sts

encode :: (Eq a) => [a] -> [(a, Int)]
encode [] = []
encode a = ecode a []
  where
    ecode (x : xs) ((y, n) : ys) | x == y = ecode (xs) ((x, n + 1) : ys)
    ecode (x : xs) ys = ecode (xs) ((x, 1) : ys)
    ecode _ ys = reverse ys

data ItemE a = One a | Multi a Int deriving (Show)

encodeM :: (Eq a) => [a] -> [ItemE a]
encodeM [] = []
encodeM (x : xs) = ecode xs ((One x) : [])
  where
    ecode :: (Eq a) => [a] -> [ItemE a] -> [ItemE a]

    ecode (x : xs) ((One a) : ys) | x == a = ecode (xs) ((Multi a 2) : ys)
    ecode (x : xs) ((Multi a n) : ys) | x == a = ecode (xs) ((Multi a (n + 1)) : ys)
    ecode (x : xs) ys = ecode (xs) ((One x) : ys)
    ecode _ ys = reverse ys

encodeMM :: (Eq a) => [a] -> [ItemE a]
encodeMM = map helper . encode
  where
    helper (x, 1) = One x
    helper (x, n) = Multi x n

decodeM :: [ItemE a] -> [a]
decodeM = foldr (\x -> ((h x) ++)) []
  where
    h (One a) = [a]
    h (Multi a n) = replicate n a

decodeMM :: [ItemE a] -> [a]
decodeMM = concatMap h
  where
    h (One a) = [a]
    h (Multi a n) = replicate n a

dupl :: [a] -> [a]
dupl = concatMap (replicate 2)

repl :: Int -> [a] -> [a]
repl n = concatMap (replicate n)

dp1 :: [a] -> Int -> [a]
dp1 a n = d a n n
  where
    --                                        d::[a]->Int->Int->[a]
    d (x : xs) n0 n1 | n1 == 0 = d (xs) n0 n
    d (x : xs) n0 n1 | n1 > 0 = x : d (xs) n0 (n1 -1)
    d (x : xs) n0 n1 | n1 < 0 = error "fdgfdgdfg"
    d x _ _ = x

dp2 :: [a] -> Int -> [a]
dp2 a n = d a n
  where
    d (x : xs) 0 = d (xs) n
    d (x : xs) n1 = x : d (xs) (n1 -1)
    d x _ = x

sp :: [a] -> Int -> [[a]]
sp a n = [take n a, drop n a]

sl :: [a] -> Int -> Int -> [a]
sl a n nn = take (nn - n) (drop n a)

sl1 :: Int -> Int -> [a] -> [a]
sl1 n nn a = drop n (take nn a)

rot :: [a] -> Int -> [a]
rot a n | n > 0 = (drop n a) ++ (take n a)
rot a n | n < 0 = (drop l a) ++ (take l a) where l = n + (length a)
rot a n | n == 0 = a

remo :: Int -> [a] -> (a, [a])
remo n xs = (xs !! n, (take n xs) ++ (drop (n + 1) xs))

remo1 :: Int -> [a] -> (a, [a])
remo1 nn xx = re nn xx (Nothing, [])
  where
    --                                     re 0 (x:xs) (w,ww)=(x,ww++xs)
    re 0 (x : xs) (w, ww) = (x, foldr (:) xs ww)
    --                                     re 0 (x:xs) (w,ww)=(x,(reverse ww)++xs)
    re n (x : xs) (w, ww) = re (n -1) xs (Nothing, x : ww)

remo2 0 (x : xs) = (x, xs)
remo2 n (x : xs) = (l, x : r)
  where
    (l, r) = remo2 (n -1) xs

ins :: a -> [a] -> Int -> [a]
ins a ass n = (take n ass) ++ (a : drop n ass)

ins1 :: a -> [a] -> Int -> [a]
ins1 a ass 0 = a : ass
ins1 a (xa : ass) n = xa : ins1 a (ass) (n -1)

ins2 :: a -> [a] -> Int -> [a]
ins2 a ass n = (take n ass) ++ [a] ++ drop n ass

ins3 :: a -> [a] -> Int -> [a]
ins3 a ass 0 = a : ass
ins3 a (xa : ass) n = xa : ins1 a (ass) (n -1)

rang :: Int -> Int -> [Int]
rang n 0 = [n]
rang n nn = n : rang (n + 1) (nn -1)

rang1 :: Int -> Int -> [Int]
rang1 n nn = take (nn - n) [n ..]

rang2 :: Int -> Int -> [Int]
rang2 n nn = [n .. nn]

--sp ::[a]->Int->[[a]]
--sp a n = [take n a, drop n a]
group :: [Int] -> [a] -> [[a]]
group [] xs = [xs]
group _ [] = [[]]
group (n : ns) xs = (take n xs) : group ns (drop n xs)
group (n : _) xs = (take n xs) : [drop n xs]

sortt :: (Ord a) => [a] -> [a]
sortt [] = []
sortt (x : xs : xss)
  | x > xs = x : sortt (xs : xss)
  | x <= xs = xs : sortt (x : xss)
sortt (x : []) = [x]

insertl :: (Ord a) => [a] -> [[a]] -> [[a]]
insertl x (xs : xss)
  | length x >length xs = x : insertl xs xss
  | otherwise = xs : insertl x xss
insertl x [] = [x]

sortl [] = []
sortl (x : xs) = insertl x (sortl xs)
myGCD::Integer->Integer->Integer
myGCD x 0=x
myGCD 0 _=0
myGCD x y=myGCD y (mod x y)





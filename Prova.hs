module Prova where

-- (++)
(+++) :: [a] -> [a] -> [a]
(+++) [] xs = xs
(+++) xs [] = xs
(+++) (x:xs) ys = x : (+++) xs ys

								

myIf :: Bool -> a -> a -> a
myIf True x y = x
myIf False x y = y

--Head

myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x:_) = Just x


--Last
myLast :: [a] -> a
myLast [] = error "empty list"
myLast (x:[]) = x
myLast (x:xs) = myLast xs


--Tail
myTail :: [a] -> [a]
myTail [] = error "empty list"
myTail (x:xs) = xs

--Lenght
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

length1 ::(Foldable t) => t a -> Int
length1 = foldl (\c _ -> c+1) 0

--Init
myInit :: [a] -> [a]
myInit [] = error "empty list"
myInit (x:[]) = []
myInit (x:xs) = x: myInit xs 

--uncons
myUncons :: [a] -> Maybe (a,[a])
myUncons [] = Nothing
myUncons (x:xs) = Just (x,xs)

--null  

myNull :: [a] -> Bool
myNull [] = True
myNull (x : xs) = False

-- map

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs


--Reverse
myReverse :: [a] ->[a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

--intersperse
myInter :: a -> [a] -> [a]
myInter p [] = []
myInter p (x:xs) = x : myUni p xs where
					myUni _ [] = []
					myUni sep (y:ys) = sep : y :myUni sep ys

--intercalate
myIntercal :: [a] -> [[a]] ->[a]
myIntercal xs xss = myConcat (myInter xs xss)

--transpose

transpose1 :: [[a]] -> [[a]]
transpose1 [] = []
transpose1 ([]: xss) = transpose1 xss
transpose1 ((x:xs): xss) = (x : [h | (h:_) <- xss]) : transpose1 (xs : [ t | (_:t) <- xss])


--subsequence
mySubsequence  :: [a] -> [[a]]
mySubsequence xs =  [] : nonEmptySubsequences xs

nonEmptySubsequences :: [a] -> [[a]]
nonEmptySubsequences []  =  []
nonEmptySubsequences (x:xs) =  [x] : foldr f [] (nonEmptySubsequences xs)
  where f ys r = ys : (x : ys) : r

--permutations
permutations            :: [a] -> [[a]]
permutations xs0        =  xs0 : perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)


--concat

cc :: [a] -> [a] -> [a]
cc [] ys = ys
cc (x:xs) ys = x : cc xs ys

myConcat2 :: [[a]] -> [a]
myConcat2 [] = []
myConcat2 (xs:xss) = xs `cc` myConcat2 xss

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

--concat1
myLength1 :: [a] -> Int
myLength1 xs = f xs 0 where
	f [] acc = acc
	f (x:xs) acc = f xs (acc + 1)

myConcat1 :: [[a]] -> [a]
myConcat1 xss = f xss [] where 
	f [] con = con
	f (x:xs) con = x `cc` f xs con
---- myIterate 
myIterate :: (a -> a) -> a -> [a]
myIterate f x =  x : myIterate f (f x)

--myRepeat
myRepeat :: a -> [a]
myRepeat x = x : myRepeat x


--myReplicate
myReplicate :: Int -> a -> [a]
myReplicate n x =  take n (myRepeat x)

--myCycle
myCycle :: [a] -> [a]
myCycle [] = []
myCycle xs = foldr (:) [] (xs ++ myCycle xs)

--myTake
myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake t (x:xs) = x : myTake (t-1) xs


--myDrop 
myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 xs = xs
myDrop t (x:xs) = myDrop (t-1) xs

--
isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ =  True
isPrefixOf _  [] =  False
isPrefixOf (x:xs)(y:ys)= x == y && isPrefixOf xs ys

insert :: (Ord a) => a -> [a] -> [a]
insert t [] = [t]
insert t [x] = if (x > t) then (t: [x]) else (x:[t])
insert t (x:xs) = if(t > x) then (x:insert t xs) else (t:insert x xs)

insertSort :: (Ord a) => [a] -> [a]
insertSort [] = []
insertSort [x] =[x]
insertSort (x:xs) = insert x (insertSort xs)

myMin ::(Ord a) => [a] -> a -> a
myMin [] x = x
myMin (x:xs) t = if( t < x) then (myMin xs t) else (myMin xs x)

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf []    _                    = True
isSubsequenceOf _     []                   = False
isSubsequenceOf a@(x:a') (y:b) | x == y    = isSubsequenceOf a' b
                               | otherwise = isSubsequenceOf a b

mySuv :: (Eq a) => [a] -> [a] -> Bool
mySuv [] xs = True
mySuv xs [] = False
mySuv l@(x:xs) (y:ys) = if(x==y) then (mySuv xs ys) else (mySuv l ys)


primos ::[Int] -> [Int]
primos [] = []
primos (x:xs) = if(divide x x) then (x:primos xs) else (primos xs)					

divide :: Int -> Int -> Bool
divide _ 0 = True
divide x y = if(x==y || y==1) then(divide x (y-1)) 
							  else
				              (if ((mod x y == 0)) then (False) 
				                     else (divide x (y-1)))

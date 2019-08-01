module Avl where

import Auxiliar (myIf)

data AVLtree k v t = Empty
				| Branch k v t (AVLtree k v t) (AVLtree k v t) 

data MyOrdering = L | E | G

empty :: AVLtree k v t
empty = Empty

isEmpty :: AVLtree k v t -> Bool
isEmpty Empty = True
isEmpty (Branch key val bal lt rt) = False

ltree :: AVLtree k v t -> AVLtree k v t
ltree Empty = Empty
ltree (Branch key' val' bal' lt rt) = lt

rtree :: AVLtree k v t -> AVLtree k v t
rtree Empty = Empty
rtree (Branch key' val' bal' lt rt) = rt

getVal :: AVLtree k v t -> Maybe v
getVal Empty = Nothing
getVal (Branch key val balance lt rt) = Just val

isEmptyLTree :: AVLtree k v t -> Bool
isEmptyLTree Empty = True
isEmptyLTree (Branch key val bal Empty rt) = True
isEmptyLTree (Branch key val bal lt rt) = False

isEmptyRTree :: AVLtree k v t -> Bool
isEmptyRTree Empty = True
isEmptyRTree (Branch key val bal lt Empty) = True
isEmptyRTree (Branch key val bal lt rt) = False

myCmp :: Ord a => a -> a -> MyOrdering
myCmp x y = myIf (x < y) L
                 (myIf (x > y) G E)

insert :: (Ord k, Num t) => AVLtree k v t -> (k, v) -> AVLtree k v t
insert Empty (key, val) = Branch key val 0 empty empty 
insert tree@(Branch key' val' b lt rt) (key, val) = decide (myCmp key key') where
                                          decide L = Branch key' val' bal' lt' rt
                                          decide E = tree
                                          decide G = Branch key' val' bal'' lt rt'
                                          lt' = insert lt (key, val)
                                          rt' = insert rt (key, val)
                                          bal' = myAltura lt'
                                          bal'' = myAltura rt'

remove :: Ord k => AVLtree k v t -> k -> AVLtree k v t
remove Empty key' = Empty
remove tree@(Branch key val bal lt rt) key' = decide (myCmp key' key) where
                                     decide L = Branch key val bal lt' rt
                                     decide G = Branch key val bal lt rt'
                                     decide E = rm tree
                                     lt' = remove lt key'
                                     rt' = remove rt key'
                                     rm (Branch key val bal Empty Empty) = Empty
                                     rm (Branch key val bal lt Empty) = lt
                                     rm (Branch key val bal Empty rt) = rt
                                     rm (Branch key val bal lt rt) = Branch key'' val'' bal lt'' rt
                                     (key'', val'') = mostRight lt
                                     mostRight (Branch key val bal lt Empty) = (key, val)
                                     mostRight (Branch key val bal lt rt) = mostRight rt
                                     lt'' = remove lt key''

search :: Ord k => AVLtree k v t -> k -> Maybe v
search Empty key = Nothing
search (Branch key' val' bal lt rt) key = decide (myCmp key key') where
                                      decide L = search lt key
                                      decide E = Just val'
                                      decide G = search rt key

extreme :: (AVLtree k v t -> Bool) -> (AVLtree k v t -> AVLtree k v t) -> AVLtree k v t -> Maybe v
extreme isEmptySubTree descend Empty = Nothing
extreme isEmptySubTree descend tree = myIf
                                      (isEmptySubTree tree)
                                      (getVal tree)
                                      (extreme isEmptySubTree descend (descend tree))

myMax :: AVLtree k v t -> Maybe v
myMax = extreme isEmptyRTree rtree

myMin :: AVLtree k v t -> Maybe v
myMin = extreme isEmptyLTree ltree

myAltura :: (Num a) => AVLtree k v t -> a
myAltura Empty = 0
myAltura (Branch key val bal Empty Empty) = 0
myAltura (Branch key val bal lt Empty) = (myAltura lt) - (-1)
myAltura (Branch key val bal Empty rt) = (-1) - (myAltura rt)
myAltura (Branch key val bal lt rt) = (myAltura lt) - (myAltura rt)

myBalance :: AVLtree k v t -> Bool
myBalance Empty = True
myBalance (Branch key val bal Empty Empty) = True
myBalance (Branch key val bal Empty rt) = myIf((myAltura Empty - myAltura rt) > 1 ||
												  (myAltura Empty - myAltura rt) < 0 )
												  (False)
												  (True)
myBalance (Branch key val bal lt Empty) = myIf((myAltura lt - myAltura Empty) > 1 ||
												  (myAltura lt - myAltura Empty) < 0 )
												  (False)
												  (True)
myBalance (Branch key val bal lt rt) = myIf((myAltura lt - myAltura rt) > 1 ||
												  (myAltura lt - myAltura rt) < 0 )
												  (False)
												  (True)

rotateLeft :: AVLtree k v t -> AVLtree k v t
rotateLeft = undefined

rotateRight :: AVLtree k v t -> AVLtree k v t
rotateRight = undefined

rotateLeftRight :: AVLtree k v t -> AVLtree k v t
rotateLeftRight = undefined


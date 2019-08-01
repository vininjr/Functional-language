module SeqTree (SeqTree, empty, size, search, insert, remove) where

import Aula3 (myIf)

data SeqTree a = Empty
               | Leaf a
               | Branch Int Int Int (SeqTree a) (SeqTree a)

size :: SeqTree a -> Int
size Empty = 0
size (Leaf x) = 1
size (Branch s c h lt rt) = s

instance Functor SeqTree where
		fmap f (Empty) = Empty
		fmap f (Leaf x) = leaf (f x)
		fmap f (Branch s c h Empty Empty) = Empty
		fmap f (Branch s c h lt Empty) = Branch s c h (fmap f lt) Empty
		fmap f (Branch s c h Empty rt) = Branch s c h  Empty (fmap f rt)
		fmap f (Branch s c h lt rt) = Branch s c h  (fmap f lt) (fmap f rt)   

capacity :: SeqTree a -> Int
capacity Empty = 0
capacity (Leaf x) = 1
capacity (Branch s c h lt rt) = c

height :: SeqTree a -> Int
height Empty = 0
height (Leaf x) = 0
height (Branch s c h lt rt) = h

empty :: SeqTree a
empty = Empty

leaf :: a -> SeqTree a
leaf x = Leaf x

makeBranch :: SeqTree a -> SeqTree a -> SeqTree a
makeBranch Empty Empty = empty
makeBranch lt Empty = lt
makeBranch Empty rt = rt
makeBranch lt rt = Branch s c h lt rt where
                   s = size lt + size rt
                   c = 2^h
                   h = max (height lt) (height rt) + 1

search :: SeqTree a -> Int -> Maybe a
search Empty n = Nothing
search (Leaf x) 1 = Just x
search (Leaf x) n = Nothing
search (Branch s c h lt rt) n = myIf (n <= hc)
                                     (search lt n)
                                     (search rt (n - hc)) where
                                hc = c `div` 2

insert :: SeqTree a -> a -> SeqTree a
insert Empty y = leaf y
insert l@(Leaf x) y = makeBranch l (leaf y)
insert b@(Branch s c h lt rt) y = myIf (s >= c)
                                     (makeBranch b (leaf y))
                                     (myIf (s < hc)
                                           (makeBranch lt' rt)
                                           (makeBranch lt rt')) where
                                  hc = c `div` 2
                                  lt' = insert lt y
                                  rt' = insert rt y

remove :: SeqTree a -> SeqTree a
remove Empty = empty
remove (Leaf x) = empty
remove (Branch s c h Empty Empty) = empty
remove (Branch s c h lt Empty) = makeBranch (remove lt) empty
remove (Branch s c h lt (Leaf x)) = makeBranch lt empty
remove (Branch s c h lt rt) = makeBranch lt (remove rt)


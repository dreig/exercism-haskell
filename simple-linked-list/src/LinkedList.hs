module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = EmptyList | Cons a (LinkedList a) deriving (Eq, Show)

datum :: LinkedList a -> a
datum EmptyList = error "cannot get datum on an empty list"
datum (Cons x _) = x

fromList :: [a] -> LinkedList a
fromList = foldr Cons EmptyList

isNil :: LinkedList a -> Bool
isNil EmptyList = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new x linkedList = x `Cons` linkedList

next :: LinkedList a -> LinkedList a
next EmptyList = error "cannot get next of an empty list"
next (Cons _ xs) = xs

nil :: LinkedList a
nil = EmptyList

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = fromList . reverse . toList

toList :: LinkedList a -> [a]
toList EmptyList = []
toList (Cons x xs) = x : toList xs

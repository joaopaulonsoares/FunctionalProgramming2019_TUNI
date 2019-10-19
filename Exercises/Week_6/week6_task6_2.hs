{--
 Implement the phone book of 4.4 using a binary tree (see https://www.sis.uta.fi/~csjtn/fp/ or the book) that stores the name as 
 the key and the phone list as the value in the node. If the name already exists, add the phone into the list. If not, make a new 
 node. This way, your node would look like

Node String [Phone] Tree Tree
Find the place for addition like in the given example (https://www.sis.uta.fi/~csjtn/fp/ ) so that the tree remains ordered. 
No need to make deletions or updates to the numbers.
--}
-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- data List' a = Empty | Cons {listHead :: a, listTail :: List' a} deriving (Show, Read, Eq, Ord)

-- binary tree:
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show,Read,Eq)

singleton x = Node x EmptyTree EmptyTree

treeInsert x EmptyTree = singleton x
treeInsert x (Node y left right)
  | x==y = Node y left right -- no duplicate keys
  | x<y = Node y (treeInsert x left) right
  | x>y = Node y left (treeInsert x right)

treeElem x EmptyTree = False
treeElem x (Node y left right)
  | x==y = True
  | x<y  = treeElem x left
  | x>y  = treeElem x right



module CaseLift.Tree where
data Tree a = Leaf a | Node (Tree a) (Tree a)
treeSize t = case t of
  Leaf x -> 1
  Node l r -> treeSize l + treeSize r

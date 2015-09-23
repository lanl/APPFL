data Tree = Leaf Int | Node Tree Tree

countLeaves :: Tree -> Int
countLeaves (Node l r) = countLeaves l + countLeaves r
countLeaves (Leaf e) = 1

countNode :: Tree -> Int
countNode (Node l r) = 1 + countNode l + countNode r
countNode (Leaf e) = 0

leavesMinusNodes :: Tree -> Int
leavesMinusNodes t = countLeaves t - countNode t  

--Induct on number of nodes
{-Base Case: number of nodes is 0
  leavesMinusNodes t
    {apply leavesMinusNodes}
= countLeaves t - countNode t
   {apply countLeaves and countNode (t must be a leaf)}
= 1 - 0
   {simplify}
= 1
-}

{-Inductive Case: Assume that leavesMinusNodes evaluates to 1 for Trees containing n nodes. We will prove that it holds for Trees containing n+1 nodes.

  leavesMinusNodes t 
   {apply leavesMinusNodes}
= countLeaves t - countNode t
  {substitute t= node l r because t can't be a leaf}
= countLeaves (Node l r) - countNode (Node l r)
  {apply countLeaves and CountNode}
= (countLeaves l + countLeaves r) - (1 + countNode l + countNode r)
  {rearrange using basic field axioms of the real numbers}
= (countLeaves l - countNode l) + (countLeaves r - countNode r) -1
  {apply leavesMinusNodes}
= leavesMinusNodes l + leavesMinusNodes r - 1
  {apply inductive hypothesis}
= 1+1-1
  {simplify}
= 1
-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns       #-}

module SCC (
  scc
) where

{-
   David King and John Launchbury, Lazy Depth-First Search and Linear Graph Algorithms in Haskell

    Structuring depth-first search algorithms in Haskell,
    ACM Principles of Programming Languages, San Francisco, 1995.
-}

import AST
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype Graph v = Graph (Map.Map v [v])

type Edge v     = (v,v)
type Node v     = (v,[v])

nodes :: Graph v -> [Node v]
nodes (Graph g) = Map.toList g

graph :: Ord v => [Node v] -> Graph v
graph es = Graph (Map.fromListWith (++) es)

edges :: Graph v -> [Edge v]
edges g
  = [(v,w) | (v,vs) <- nodes g, w <- vs]

vertices :: Graph v -> [v]
vertices g
  = [v | (v,vs) <- nodes g]

successors :: Ord v => v -> Graph v -> [v]
successors v (Graph g)
  = Map.findWithDefault [] v g

transpose :: Ord v => Graph v -> Graph v
transpose g@(Graph m)
  = Graph (foldr add empty (edges g))
  where
    empty       = Map.map (const []) m
    add (v,w) m = Map.adjust (v:) w m

--  Depth first search and forests

data Tree v   = Node v (Forest v)
type Forest v = [Tree v]

dff :: Ord v => Graph v -> Forest v
dff g = dfs g (vertices g)

dfs :: Ord v => Graph v -> [v] -> Forest v
dfs g vs = prune (map (tree g) vs)

tree :: Ord v => Graph v -> v -> Tree v
tree g v = Node v (map (tree g) (successors v g))

prune :: Ord v => Forest v -> Forest v
prune fs
  = snd (chop Set.empty  fs)
  where
    chop ms []  = (ms,[])
    chop ms (Node v vs:fs)
      | visited   = chop ms fs
      | otherwise = let ms0       = Set.insert v ms
                        (ms1,vs') = chop ms0 vs
                        (ms2,fs') = chop ms1 fs
                    in (ms2,Node v vs':fs')
      where
        visited   = Set.member v ms

--  Orderings

preorder :: Ord v => Graph v -> [v]
preorder g = preorderF (dff g)

preorderF fs = concatMap preorderT fs

preorderT (Node v fs) = v:preorderF fs

postorder :: Ord v => Graph v -> [v]
postorder g = postorderF (dff g)

postorderT t = postorderF [t]

postorderF ts
  = postorderF' ts []
  where
    -- efficient concatenation
    postorderF' [] tl          = tl
    postorderF' (t:ts) tl      = postorderT' t (postorderF' ts tl)
    postorderT' (Node v fs) tl = postorderF' fs (v:tl)

scc :: Ord v => [(v,[v])] -> [[v]]
scc nodes = sccG (graph nodes)

sccG :: Ord v => Graph v -> [[v]]
sccG g = map preorderT (sccF g)

sccF :: Ord v => Graph v -> Forest v
sccF g = reverse (dfs (transpose g) (topsort g))

topsort g = reverse (postorder g)

--  Reachable and path

reachable v g
  = preorderF (dfs g [v])

path v w g
  = elem w (reachable v g)

--  Show

instance Show v => Show (Graph v) where
  showsPrec d (Graph m) = shows m
  
instance Show v => Show (Tree v) where
  showsPrec d (Node v []) = shows v
  showsPrec d (Node v fs) = shows v . showList fs


--  Quick Test

tgraph0 :: Graph Int
tgraph0 = graph
          [(0,[1])
          ,(1,[2,1,3])
          ,(2,[1])
          ,(3,[])
          ]

tgraph1 = graph
          [  ('a',"jg")
          ,  ('b',"ia")
          ,  ('c',"he")
          ,  ('d',"")
          ,  ('e',"jhd")
          ,  ('f',"i")
          ,  ('g',"fb")
          ,  ('h',"")
          ]

-- transform per sccs -- BAD IDEA:  makes tlds no longer tlds

-- this could change depending when this is done
-- don't need all of this
{-
class GetFvs a where
    getFvs :: a -> Set.Set Var
    putFvs :: (Set.Set Var) -> a -> a
instance GetFvs (Obj [Var]) where
    getFvs o = Set.fromList $ omd o
    putFvs vs o = o{omd = Set.toList vs}
instance GetFvs (Expr [Var]) where
    getFvs e = Set.fromList $ emd e
    putFvs vs e = e{emd = Set.toList vs}
instance GetFvs (Alts [Var]) where
    getFvs as = Set.fromList $ altsmd as
    putFvs vs as = as{altsmd = Set.toList vs}
instance GetFvs (Alt [Var]) where
    getFvs a = Set.fromList $ amd a
    putFvs vs a = a{amd = Set.toList vs}

getFvsObj :: Obj InfoTab -> Set.Set Var
getFvsObj o = Set.fromList $ omd $ truefvs o

getFvsExpr :: Expr InfoTab -> Set.Set Var
getFvsExpr e = Set.fromList $ emd $ truefvs e

putFvsExpr :: Set.Set Var -> Expr InfoTab -> Expr InfoTab
putFvsExpr fvs e = let md = emd e
                       md' = md{truefvs = Set.toList fvs}

class SCCDepTrans a where
    dt :: a -> a

instance SCCDepTrans (Obj [Var]) where
    dt o@FUN{e} = o{e = dt e}
    dt o@THUNK{e} = o{e = dt e}
    dt o = o

instance SCCDepTrans (Expr [Var]) where
    dt e@ECase{ee, ealts} = e{ee = dt ee, ealts = dt ealts}
    dt ELet{edefs, ee} =
        let ee' = dt ee
            edefs' = dt edefs
            allfvsl = map getFvsObj edefs'
            namel = map oname edefs'
            localfvss = map (Set.intersection $ Set.fromList namel) allfvsl
            localfvsl = map Set.toList localfvss
            sccs = scc $ zip namel localfvsl
            -- translate list of lists of onames to list of lists of Objs
            nameobjmap = Map.fromList $ zip namel edefs'
            sccsobj = (map . map) (findItOrElse nameobjmap) sccs
            fl = map (\edefs ->
                          \ee ->
                              let onames = Set.fromList $ map oname edefs
                                  fvs = foldr Set.union
                                              (getFvsExpr ee)
                                              (map getFvsObj edefs)
                                  fvs' = Set.difference fvs onames
                              in putFvsExpr fvs' ELet{emd = ["i n v a l i d"], -- invalid Var
                                                      edefs = edefs,
                                                      ee = ee})
                     sccsobj
        in (foldr1 (.) fl) ee'
    -- all the rest
    dt e = e

findItOrElse map x =
    case Map.lookup x map of
      Nothing -> error $ "findItOrElse can't find " ++ x ++ " in " ++ show map
      Just y -> y

instance SCCDepTrans a => SCCDepTrans [a] where
    dt = map dt

instance SCCDepTrans (Alts [Var]) where
    dt as@Alts{alts} = as{alts = map dt alts}

instance SCCDepTrans (Alt [Var]) where
    dt a = a{ae = dt $ ae a}
-}

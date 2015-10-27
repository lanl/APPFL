{-# LANGUAGE FlexibleInstances #-}
module Language.C.DSL.Stat where
import Language.C

-- | An if statement with no else clause
cif :: CExpr -> CStat -> CStat
cif exp stat = CIf exp stat Nothing undefNode

-- | An if statement with an else clause
cifElse :: CExpr -> CStat -> CStat -> CStat
cifElse exp th el = CIf exp th (Just el) undefNode

-- | A while loop, the 'CExpr' is the looping condition.
while :: CExpr -> CStat -> CStat
while exp stat = CWhile exp stat False undefNode

-- | A for loop, an example use
-- 
-- > for(int "x" .= 1, 1, PlusPlus `pre` "x")
-- >   "printf"#[str "%d\n", "x"]
for :: (CDecl, CExpr, CExpr) -> CStat -> CStat
for (init, test, upd) block = CFor (Right init) (Just test) (Just upd) block undefNode

-- | A for loop with no declarations.
noDeclFor :: (CExpr, CExpr) -> CStat -> CStat
noDeclFor (test, upd) block = CFor (Left Nothing) (Just test) (Just upd) block undefNode

-- | A do while loop.
doWhile :: CExpr -> CStat -> CStat
doWhile exp stat = CWhile exp stat True undefNode

cbreak :: CStat
cbreak = CBreak undefNode

ccont :: CStat
ccont = CCont undefNode

creturn :: CExpr -> CStat
creturn = flip CReturn undefNode . Just

cvoidReturn :: CStat
cvoidReturn = CReturn Nothing undefNode

liftE :: CExpr -> CStat
liftE e = CExpr (Just e) undefNode

class BlockLike a where
  intoB :: a -> CBlockItem
instance BlockLike CStat where
  intoB = CBlockStmt
instance BlockLike CExpr where
  intoB = intoB . liftE
instance BlockLike CDecl where
  intoB = CBlockDecl

block :: [CBlockItem] -> CStat
block = flip (CCompound []) undefNode

hBlock :: BlockLike a => [a] -> CStat
hBlock = block . map intoB


type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

data Prop =  Const Bool
           | Var Char
           | Not Prop
           | And Prop Prop 
           | Or Prop Prop
           | Imply Prop Prop
           | Equiv Prop Prop

p1 :: Prop
p1 = And (Var 'A')(Not (Var 'A'))
p2 :: Prop
p2=Imply(And(Var 'A')(Var 'B'))(Var 'A')
p3 :: Prop 
p3 = Imply (Var 'A')(And (Var 'A')(Var 'B'))
p4 :: Prop
p4 = Imply (And (Var 'A')(Imply (Var 'A')(Var 'B')))(Var 'B')
p5 :: Prop 
p5 = Or (Var 'A')(Var 'B')
p6 :: Prop 
p6 = Or (Not (Var 'B'))(Var 'B')
p7 :: Prop
p7 = Equiv (Var 'A')(Var 'B')
p8 :: Prop
p8 = Equiv (Var 'A')(Var 'A')


eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s 
eval s (Not p) = not(eval s p)
eval s (And p q) = (eval s p) && (eval s q)
eval s (Or p q) = (eval s p) || (eval s q)
eval s (Imply p q) = (eval s p)<= (eval s q)
eval s (Equiv p q) = ((eval s p) <= (eval s q)) && ((eval s q) <= (eval s p))

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools (n) = map (False:) bss ++ map (True:)bss
            where bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and[eval s p | s <- substs p]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v|(k',v) <- t, k==k']

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x:rmdups (filter (/= x) xs)

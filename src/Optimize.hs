module Optimize where

import Common (Pos (NoPos))
import Lang (BinaryOp (..), Const (..), Decl (..), Scope (Sc1), Scope2 (Sc2), TTerm, Tm (..), Ty (NatTy), Var (..))
import Subst (subst, open, open2, close2, close)
import Debug.Trace (trace)

type Optimizer = TTerm -> (Bool, TTerm)

type Optimizers = [Optimizer]

recOptimizer :: Optimizer -> TTerm -> (Bool, TTerm)
recOptimizer f t@(V _ _) = (False, t)
recOptimizer f t@(Const _ _) = (False, t)
recOptimizer f (Lam i n ty t) = let (c, ot) = f $ open n t 
  in (c, Lam i n ty $ close n ot)
recOptimizer f (App i l r) =
  let (c, ol) = f l
      (c', orv) = f r
   in (c || c', App i ol orv)
recOptimizer f (Print i s t) =
  let (c, ot) = f t
   in (c, Print i s ot)
recOptimizer f (BinaryOp i op l r) =
  let (c, ol) = f l
      (c', orv) = f r
   in (c || c', BinaryOp i op ol orv)
recOptimizer f (Fix i n ty n' ty' t) =
  let (c, ot) = f $ open2 n' n t
   in (c, Fix i n ty n' ty' $ close2 n' n ot)
recOptimizer f (IfZ i ct t e) =
  let (c, oc) = f ct
      (c', ot) = f t
      (c'', oe) = f e
   in (c || c' || c'', IfZ i oc ot oe)
recOptimizer f (Let i n ty v t) =
  let (c, ov) = f v
      (c', ot) = f (open n t)
   in (c || c', Let i n ty ov $ close n ot)

runBinaryOp :: BinaryOp -> Const -> Const -> Const
runBinaryOp Add (CNat a) (CNat b) = CNat $ a + b
runBinaryOp Sub (CNat a) (CNat b) = CNat $ a - b

consfold :: TTerm -> (Bool, TTerm)
consfold (BinaryOp _ op (Const _ a) (Const _ b)) = (True, Const (NoPos, NatTy) (runBinaryOp op a b))
consfold (BinaryOp _ Add (Const _ (CNat 0)) a) = (True, snd (consfold a))
consfold (BinaryOp _ Sub a (Const _ (CNat 0))) = (True, snd (consfold a))
consfold (IfZ _ (Const _ (CNat a)) t e)
  | a == 0 = (True, snd (consfold t))
  | otherwise = (True, snd (consfold e))
consfold t = recOptimizer consfold t

consprop :: TTerm -> (Bool, TTerm)
consprop (Let _ _ _ c@(Const _ _) s) = (True, snd (consprop (subst c s)))
consprop t = recOptimizer consprop t

deadcodeelim :: TTerm -> (Bool, TTerm)
deadcodeelim tt@(Let _ name _ _ (Sc1 b)) = if 
    go 0 (trace (show b) b) then
    recOptimizer deadcodeelim tt else
    (True, snd $ deadcodeelim (open name (Sc1 b)))
  where
    go n (V p (Bound i))
      | i == n = True
      | otherwise = False
    go n (V p (Free x)) = False
    go n (V p (Global x)) = False
    go n t@(Const _ _) = False
    go n (Lam p y ty (Sc1 t)) = go (n + 1) t
    go n (App p l r) = go n l || go n r
    go n (Fix p f fty x xty (Sc2 t)) = go (n + 2) t
    go n (IfZ p c t e) = go n c || go n t || go n e
    go n (Print p str t) = go n t
    go n (BinaryOp p op t u) = go n t || go n u
    go n (Let p v vty m (Sc1 o)) = go n m || go (n + 1) o
deadcodeelim l = recOptimizer deadcodeelim l

optimizeRec :: Optimizers -> Bool -> TTerm -> TTerm
optimizeRec ops c t
  | c =
      let ott = foldr (\op (cond, tt) -> let (c', t') = op tt in (cond || c', t')) (False, t) ops
       in uncurry (optimizeRec ops) ott
  | otherwise = t

optimize :: TTerm -> TTerm
optimize = optimizeRec [deadcodeelim, consprop, consfold] True

optimizeDecl :: Decl TTerm -> Decl TTerm
optimizeDecl (Decl p x tt) = Decl p x $ optimize tt
optimizeDecl t = t
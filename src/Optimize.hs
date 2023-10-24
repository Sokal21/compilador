module Optimize where

import Lang (TTerm, BinaryOp (..), Tm (..), Const (..), Ty (NatTy), Scope (Sc1), Scope2 (Sc2), Decl (..))
import Common (Pos(NoPos))
import Subst (subst)

type Optimizer = TTerm -> (Bool, TTerm)

type Optimizers = [Optimizer]

recOptimizer :: Optimizer -> TTerm -> (Bool, TTerm)
recOptimizer f t@(V _ _) = (False, t)
recOptimizer f t@(Const _ _) = (False, t)
recOptimizer f (Lam i n ty (Sc1 t)) = let (c, ot) = recOptimizer f t in (c, Lam i n ty $ Sc1 ot)
recOptimizer f (App i l r) = let
    (c, ol) = f l
    (c', orv) = f r
    in (c || c', App i ol orv)
recOptimizer f (Print i s t) = let
    (c, ot) = f t
    in (c,Print i s ot)
recOptimizer f (BinaryOp i op l r) = let
    (c, ol) = f l
    (c', orv) = f r
    in (c || c', BinaryOp i op ol orv)
recOptimizer f (Fix i n ty n' ty' (Sc2 t)) = let
    (c, ot) = f t
    in (c, Fix i n ty n' ty' $ Sc2 ot)
recOptimizer f (IfZ i ct t e) = let
    (c, oc) = f ct
    (c', ot) = f t
    (c'', oe) = f e
    in (c || c' || c'', IfZ i oc ot oe)
recOptimizer f (Let i n ty v (Sc1 t)) = let
    (c, ov) = f v
    (c', ot) = f t
    in (c || c', Let i n ty ov $ Sc1 ot)

runBinaryOp :: BinaryOp -> Const -> Const -> Const
runBinaryOp Add (CNat a) (CNat b) = CNat $ a + b
runBinaryOp Sub (CNat a) (CNat b) = CNat $ a - b

consfold :: TTerm -> (Bool, TTerm)
consfold (BinaryOp _ op (Const _ a) (Const _ b)) = (True, Const (NoPos, NatTy) (runBinaryOp op a b))
consfold (BinaryOp _ Add (Const _ (CNat 0)) a) = (True, snd (consfold a))
consfold (BinaryOp _ Sub a (Const _ (CNat 0))) = (True, snd (consfold a))
consfold (IfZ _ (Const _ (CNat a)) t e) | a == 0 = (True, snd (consfold t))
                                        | otherwise = (True,  snd (consfold e))
consfold t = recOptimizer consfold t

consprod :: TTerm -> (Bool, TTerm)
consprod (Let _ _ _ c@(Const _ _) s) = (True, snd (consprod (subst c s)))
consprod t = recOptimizer consprod t

optimizeRec :: Optimizers -> Bool -> TTerm -> TTerm
optimizeRec ops c t | c = let
                            ott = foldr (\op (c, t) -> let (c', t') = op t in (c || c', t')) (False, t) ops
                            in uncurry (optimizeRec ops) ott
                    | otherwise = t

optimize :: TTerm -> TTerm
optimize = optimizeRec [consprod, consfold] True

optimizeDecl :: Decl TTerm -> Decl TTerm
optimizeDecl (Decl p x tt) = Decl p x $ optimize tt
optimizeDecl t = t
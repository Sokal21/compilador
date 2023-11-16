module ClosureConvert where

import Control.Monad.State (StateT (runStateT), MonadState (get, put))
import Control.Monad.Writer
import IR (Ir (..), IrDecl (IrVal, IrFun), IrTy (IrClo, IrFunTy, IrInt))
import Lang (Name, Scope (Sc1), Scope2 (Sc2), TTerm (..), Tm (..), Ty (..), Var (..), Decl (..), getTy)
import Subst (open, open2)
import qualified Data.Bifunctor

type CHMonad = StateT Int (Writer [IrDecl])

freshName :: String -> CHMonad String
freshName n = do
  i <- get
  put (i + 1)
  return (n ++ "__" ++ show i)

funFreeVarToIrLet :: Name -> [(Name, IrTy)] -> Ir -> Ir
funFreeVarToIrLet = go 1
  where
    go :: Int -> Name -> [(Name, IrTy)] -> Ir -> Ir
    go _ clos [] b = b
    go i clos ((v, ty):vs) b = IrLet v ty (IrAccess (IrVar clos) ty i) (go (i+1) clos vs b)


closureConvert :: TTerm -> CHMonad Ir
closureConvert (Const info c) = return $ IrConst c
closureConvert (Print info n t) = do
  i <- closureConvert t
  fp <- freshName "print"

  return $ IrLet fp IrInt i (IrPrint n (IrVar fp))
closureConvert (BinaryOp info op l r) = do
  il <- closureConvert l
  ir <- closureConvert r
  return $ IrBinaryOp op il ir
closureConvert (IfZ info c t e) = do
  ic <- closureConvert c
  it <- closureConvert t
  ie <- closureConvert e
  return $ IrIfZ ic it ie
closureConvert (V info (Global n)) = return $ IrGlobal n
closureConvert (V info (Free n)) = return $ IrVar n
closureConvert (V info (Bound i)) = error "TODO"
closureConvert (App (_, ty) f t) = do
  fc <- freshName "app"
  f' <- closureConvert f
  it <- closureConvert t
  return $ IrLet fc IrClo f' $ IrCall (IrAccess (IrVar fc) IrFunTy 0) [IrVar fc, it] (typeConversion ty)
closureConvert t@(Lam (_, fty) n ty sc) = do
  fn <- freshName ("fun__" ++ n)
  fc <- freshName "clos_lam"
  let osc = open n sc
  iosc <- closureConvert osc
  let free = findFreeVariable t
  let freeVars = map (Data.Bifunctor.second typeConversion) free
  tell [IrFun fn ((typeConversion . codom) fty) [(fc, IrClo), (n, typeConversion ty)] (funFreeVarToIrLet fc freeVars iosc)]
  return $ MkClosure fn $ map (IrVar . fst) freeVars
closureConvert (Let _ n ty vt dt) = do
  ivt <- closureConvert vt
  let osc = open n dt
  iosc <- closureConvert osc
  return $ IrLet n (typeConversion ty) ivt iosc
closureConvert t@(Fix _ nf ty nx tyx bt) = do
  fn <- freshName $ "fix__" ++ nf ++ "_" ++ nx
  fc <- freshName "fix_clos"
  let osc = open2 nf nx bt
  iosc <- closureConvert osc
  let free = findFreeVariable t
  let freeVars = map (Data.Bifunctor.second typeConversion) free
  -- capaz no hace falta el let de adentro
  tell [IrFun fn ((typeConversion . codom) ty) [(fc, IrClo), (nx, typeConversion tyx)] (IrLet nf (typeConversion ty) (IrVar fc) (funFreeVarToIrLet fc freeVars iosc))]
  return $ MkClosure fn $ map (IrVar . fst) freeVars

uniqueElements :: (Eq a) => [a] -> [a]
-- TODO: no anda bien
uniqueElements xs = filter (`elem` xs) xs

codom :: Ty -> Ty
codom NatTy = NatTy
codom (FunTy _ ty) = ty

typeConversion :: Ty -> IrTy
typeConversion NatTy = IrInt
typeConversion (FunTy _ _) = IrFunTy
-- TODO: Es necesario buscar en un 'estado' las declaraciones de tipo ya definidas
typeConversion (DeclTy n) = IrFunTy

-- typeConversion DeclTy = TODO

findFreeVariable :: TTerm -> [(Name, Ty)]
findFreeVariable = uniqueElements . findFreeVariable'

findFreeVariable' :: TTerm -> [(Name, Ty)]
findFreeVariable' (Fix _ _ _ _ _ (Sc2 t)) = findFreeVariable' t
findFreeVariable' (Let _ _ _ t (Sc1 t')) = findFreeVariable' t ++ findFreeVariable' t'
findFreeVariable' (Lam _ _ _ (Sc1 t)) = findFreeVariable' t
findFreeVariable' (App _ f t) = findFreeVariable' f ++ findFreeVariable' t
findFreeVariable' (V (_, t) (Free n)) = [(n, t)]
findFreeVariable' (V (_, t) (Global n)) = [(n, t)]
findFreeVariable' (Print _ _ t) = findFreeVariable' t
findFreeVariable' (IfZ _ c t e) = findFreeVariable' c ++ findFreeVariable' t ++ findFreeVariable' e
findFreeVariable' (BinaryOp _ _ l r) = findFreeVariable' l ++ findFreeVariable' r
findFreeVariable' _ = []

convertDeclToTTerm :: Decl TTerm -> CHMonad ()
convertDeclToTTerm (Decl _ n t) = do
  i <- closureConvert t
  let ty = (typeConversion . getTy) t
  tell [IrVal n ty i]
  return ()
convertDeclToTTerm _ = return ()

closureConvertDecl :: [Decl TTerm] -> [IrDecl]
closureConvertDecl d =
  -- TODO: Por facilidad, habria que reemplazar todas las declaraciones de tipos por su definicion
  let m = mapM convertDeclToTTerm d
      (r, ds) = runWriter $ runStateT m 0

  in ds
  -- in runWriter $ runStateT m 0
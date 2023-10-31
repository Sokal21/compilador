module CEK where

import Common (Pos (NoPos), lookUpIndex)
import Data.List (intercalate)
import Lang
import MonadFD4 (MonadFD4, lookupDecl, printFD4, addStep)
import PPrint
import Subst (open, open2)

type CEKEnv = [CEKValue]

data CEKClos = CFun CEKEnv TTerm Ty | CFix CEKEnv TTerm Ty deriving (Show)

data CEKValue = CCons Const | CClos CEKClos deriving (Show)

data Frame
  = KArg CEKEnv TTerm
  | KClos CEKClos
  | KIfZ CEKEnv TTerm TTerm
  | KBinaryOpFst CEKEnv BinaryOp TTerm
  | KBinaryOpSnd CEKValue BinaryOp
  | KPrint String
  | KLet CEKEnv TTerm
  deriving (Show)

type Kont = [Frame]

seek :: (MonadFD4 m) => TTerm -> CEKEnv -> Kont -> m CEKValue
seek (Print _ s t) env kont = addStep >> seek t env (KPrint s : kont)
seek (BinaryOp _ bop lt rt) env kont =  addStep >> seek lt env (KBinaryOpFst env bop rt : kont)
seek (IfZ _ c t e) env kont = addStep >> seek c env (KIfZ env t e : kont)
seek (App _ ft vt) env kont = addStep >> seek ft env (KArg env vt : kont)
seek (V _ var) env kont = addStep >> case var of
  Bound i -> case lookUpIndex i env of
    Nothing -> error "No pudimos encontrar la variable ligada en el entorno"
    Just a -> destroy a kont
  Free name -> do
    t <- lookupDecl name
    case t of
      Nothing -> error "No pudimos encontrar el termino asociado a la variable libre"
      Just tt -> do
        st <- seek tt [] []
        destroy st kont
  Global name -> do
    t <- lookupDecl name
    case t of
      Nothing -> error "No pudimos encontrar el termino asociado a la variable global"
      Just tt -> do
        st <- seek tt [] []
        destroy st kont
seek (Const _ c) _ kont = addStep >> destroy (CCons c) kont
seek t@(Lam _ _ _ (Sc1 s)) env kont = addStep >> destroy (CClos (CFun env s (getTy t))) kont
seek t@(Fix _ _ _ n2 _ (Sc2 s)) env kont = addStep >> destroy (CClos (CFix env s (getTy t))) kont
seek (Let _ _ _ t (Sc1 s)) env kont = addStep >> seek t env (KLet env s : kont)

destroy :: (MonadFD4 m) => CEKValue -> Kont -> m CEKValue
destroy v ((KPrint s) : xs) = addStep >> do
  pv <- cekValue2string v
  printFD4 $ s ++ pv
  destroy v xs
destroy v ((KBinaryOpFst env op t) : xs) = addStep >> seek t env (KBinaryOpSnd v op : xs)
destroy v' ((KBinaryOpSnd v op) : xs) = addStep >> do
  vr <- cekOperation v op v'
  destroy vr xs
destroy (CCons (CNat 0)) ((KIfZ env lt rt) : xs) = addStep >> seek lt env xs
destroy _ ((KIfZ env lt rt) : xs) = addStep >> seek rt env xs
destroy (CClos c) ((KArg env t) : xs) = addStep >> seek t env (KClos c : xs)
destroy v ((KArg env t) : xs) = error "Aplicacion de un valor"
destroy v ((KClos (CFun env t _)) : xs) = addStep >> seek t (v : env) xs
destroy v ((KClos f@(CFix env t _)) : xs) = addStep >> seek t (v : (CClos f : env)) xs
destroy v ((KLet env t) : xs) = addStep >> seek t (v : env) xs
destroy v [] = return v

cekOperation :: (MonadFD4 m) => CEKValue -> BinaryOp -> CEKValue -> m CEKValue
cekOperation (CCons (CNat l)) Add (CCons (CNat r)) = return $ CCons (CNat (l + r))
cekOperation (CCons (CNat l)) Sub (CCons (CNat r)) = return $ CCons (CNat (l - r))
cekOperation lv o rv = error "Unsuported operation"

value2term :: CEKValue -> TTerm
value2term (CCons c@(CNat _)) = Const (NoPos, NatTy) c
value2term (CClos (CFun _ te ty)) = Lam (NoPos, ty) "x" (getTy e) (Sc1 te)
  where
    e = open "x" (Sc1 te)
value2term (CClos (CFix _ te ty)) = Fix (NoPos, ty) "f" ty "x" (getTy e) (Sc2 te)
  where
    e = open2 "f" "x" (Sc2 te)

-- TODO Mirar q onda esto
cekValue2string :: (MonadFD4 m) => CEKValue -> m String
cekValue2string (CCons (CNat i)) = return $ show i
cekValue2string (CClos (CFun env t ty)) = do
  ppt <- pp t
  es <- env2string env
  return $ "Clos_fun(" ++ es ++ ", " ++ ppt ++ ")"
cekValue2string (CClos (CFix env t ty)) = do
  ppt <- pp t
  es <- env2string env
  return $ "Clos_fix(" ++ es ++ ", " ++ ppt ++ ")"

env2string :: (MonadFD4 m) => CEKEnv -> m String
env2string = (intercalate ", " <$>) . mapM cekValue2string

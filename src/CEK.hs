module CEK where

import Common (Pos (NoPos), lookUpIndex)
import Data.List (intercalate)
import Lang
import MonadFD4 (MonadFD4, lookupDecl, printFD4, addStep)
import PPrint
import Subst (open, open2)
import qualified Control.Monad

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

makeStep:: (MonadFD4 m) => Bool -> m ()
makeStep p =
  Control.Monad.when p $ do
    addStep

seek :: (MonadFD4 m) => Bool -> TTerm -> CEKEnv -> Kont -> m CEKValue
seek p (Print _ s t) env kont = makeStep p >> seek p t env (KPrint s : kont)
seek p (BinaryOp _ bop lt rt) env kont =  makeStep p >> seek p lt env (KBinaryOpFst env bop rt : kont)
seek p (IfZ _ c t e) env kont = makeStep p >> seek p c env (KIfZ env t e : kont)
seek p (App _ ft vt) env kont = makeStep p >> seek p ft env (KArg env vt : kont)
seek p (V _ var) env kont = makeStep p >> case var of
  Bound i -> case lookUpIndex i env of
    Nothing -> error "No pudimos encontrar la variable ligada en el entorno"
    Just a -> destroy p a kont
  Free name -> do
    t <- lookupDecl name
    case t of
      Nothing -> error "No pudimos encontrar el termino asociado a la variable libre"
      Just tt -> do
        st <- seek p tt [] []
        destroy p st kont
  Global name -> do
    t <- lookupDecl name
    case t of
      Nothing -> error "No pudimos encontrar el termino asociado a la variable global"
      Just tt -> do
        st <- seek p tt [] []
        destroy p st kont
seek p (Const _ c) _ kont = makeStep p >> destroy p (CCons c) kont
seek p t@(Lam _ _ _ (Sc1 s)) env kont = makeStep p >> destroy p (CClos (CFun env s (getTy t))) kont
seek p t@(Fix _ _ _ n2 _ (Sc2 s)) env kont = makeStep p >> destroy p (CClos (CFix env s (getTy t))) kont
seek p (Let _ _ _ t (Sc1 s)) env kont = makeStep p >> seek p t env (KLet env s : kont)

destroy :: (MonadFD4 m) => Bool -> CEKValue -> Kont -> m CEKValue
destroy p v ((KPrint s) : xs) = makeStep p >> do
  pv <- cekValue2string v
  printFD4 $ s ++ pv
  destroy p v xs
destroy p v ((KBinaryOpFst env op t) : xs) = makeStep p >> seek p t env (KBinaryOpSnd v op : xs)
destroy p v' ((KBinaryOpSnd v op) : xs) = makeStep p >> do
  vr <- cekOperation v op v'
  destroy p vr xs
destroy p (CCons (CNat 0)) ((KIfZ env lt rt) : xs) = makeStep p >> seek p lt env xs
destroy p _ ((KIfZ env lt rt) : xs) = makeStep p >> seek p rt env xs
destroy p (CClos c) ((KArg env t) : xs) = makeStep p >> seek p t env (KClos c : xs)
destroy p v ((KArg env t) : xs) = error "Aplicacion de un valor"
destroy p v ((KClos (CFun env t _)) : xs) = makeStep p >> seek p t (v : env) xs
destroy p v ((KClos f@(CFix env t _)) : xs) = makeStep p >> seek p t (v : (CClos f : env)) xs
destroy p v ((KLet env t) : xs) = makeStep p >> seek p t (v : env) xs
destroy p v [] = return v

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

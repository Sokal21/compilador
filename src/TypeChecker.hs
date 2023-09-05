{-|
Module      : Typechecker
Description : Chequeo de tipos de términos y declaraciones.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}
module TypeChecker (
   tc,
   tcDecl
   ) where

import Common
import Lang
import Global
import MonadFD4
import PPrint
import Subst

tcty :: MonadFD4 m => Pos -> Ty -> [(Name,Ty)] -> m Ty
tcty _ NatTy _ = return NatTy
tcty p (FunTy l r) env = do
    lt <- tcty p l env
    rt <- tcty p r env
    return (FunTy lt rt)
tcty p (DeclTy n) env = case lookup n env of
    Nothing -> failPosFD4 p $ "Tipo no declarado "++ppName n
    Just ty -> tcty p ty env


-- | 'tc' chequea y devuelve el tipo de un término 
-- Si el término no está bien tipado, lanza un error
-- usando la interfaz de las mónadas @MonadFD4@.
tc :: MonadFD4 m => Term         -- ^ término a chequear
                 -> [(Name,Ty)]  -- ^ entorno de tipado
                 -> TyEnv
                 -> m TTerm        -- ^ tipo del término
tc (V p (Bound _)) _ _ = failPosFD4 p "typecheck: No deberia haber variables Bound"
tc (V p (Free n)) bs tye = case lookup n bs of
                           Nothing -> failPosFD4 p $ "Variable no declarada "++ppName n
                           Just ty -> do
                            return (V (p,ty) (Free n))
tc (V p (Global n)) bs tye = case lookup n bs of
                           Nothing -> failPosFD4 p $ "Variable no declarada "++ppName n
                           Just ty -> do
                            return (V (p,ty) (Global n))
tc (Const p (CNat n)) _ _ = return (Const (p,NatTy) (CNat n))
tc (Print p str t) bs tye = do
      tt <- tc t bs tye
      expect NatTy tt tye
      return (Print (p, NatTy) str tt)
tc (IfZ p c t t') bs tye = do
       ttc  <- tc c bs tye
       expect NatTy ttc tye
       tt  <- tc t bs tye
       tt' <- tc t' bs tye
       let ty = getTy tt
       expect ty tt' tye
       return (IfZ (p,ty) ttc tt tt')
tc (Lam p v ty t) bs tye = do
         tt <- tc (open v t) ((v,ty):bs) tye
         return (Lam (p, FunTy ty (getTy tt)) v ty (close v tt))
tc (App p t u) bs tye = do
         tt <- tc t bs tye
         (dom,cod) <- domCod tt tye
         tu <- tc u bs tye
         expect dom tu tye
         return (App (p,cod) tt tu)
tc (Fix p f fty x xty t) bs tye = do
         (dom, cod) <- domCod (V (p,fty) (Free f)) tye
         edom <- tcty p dom tye
         exty <- tcty p xty tye
         when (edom /= exty) $ do
           failPosFD4 p "El tipo del argumento de un fixpoint debe coincidir con el \
                        \dominio del tipo de la función"
         let t' = open2 f x t
         tt' <- tc t' ((x,xty):(f,fty):bs) tye
         expect cod tt' tye
         return (Fix (p,fty) f fty x xty (close2 f x tt'))
tc (Let p v ty def t) bs tye = do
         tdef <- tc def bs tye
         expect ty tdef tye
         tt <- tc (open v t) ((v,ty):bs) tye
         return (Let (p,getTy tt) v ty tdef (close v tt))
tc (BinaryOp p op t u) bs tye = do
         tt <- tc t bs tye
         expect NatTy tt tye
         tu <- tc u bs tye
         expect NatTy tu tye
         return (BinaryOp (p,NatTy) op tt tu)

-- | @'typeError' t s@ lanza un error de tipo para el término @t@ 
typeError :: MonadFD4 m => TTerm   -- ^ término que se está chequeando  
                        -> String -- ^ mensaje de error
                        -> m a
typeError t s = do
   ppt <- pp t
   failPosFD4 (getPos t) $ "Error de tipo en "++ppt++"\n"++s

-- | 'expect' chequea que el tipo esperado sea igual al que se obtuvo
-- y lanza un error si no lo es.
-- TODO: Una buena idea seria nunca evaluar los tipos hasta que no se tenga que typechekear
expect :: MonadFD4 m => Ty    -- ^ tipo esperado
                     -> TTerm
                     -> TyEnv
                     -> m TTerm
expect ty tt tenv = let ty' = getTy tt
               in do 
                 ety' <- tcty (getPos tt) ty' tenv
                 ety <- tcty (getPos tt) ty tenv
                 if ety == ety' then return tt
                               else typeError tt $ "Tipo esperado: "++ ppTy (freshSTy ty) ++"\npero se obtuvo: "++ ppTy (freshSTy ty')

-- | 'domCod chequea que un tipo sea función
-- | devuelve un par con el tipo del dominio y el codominio de la función
domCod :: MonadFD4 m => TTerm -> TyEnv -> m (Ty, Ty)
domCod tt tye = do
    ety <- tcty (getPos tt) (getTy tt) tye 
    case ety of
        FunTy d c -> return (d, c)
        _         -> typeError tt $ "Se esperaba un tipo función, pero se obtuvo: " ++ ppTy (freshSTy (getTy tt))

-- | 'tcDecl' chequea el tipo de una declaración
-- y la agrega al entorno de tipado de declaraciones globales
tcDecl :: MonadFD4 m  => Decl Term -> m (Decl TTerm)
tcDecl (Decl p n t) = do
    --chequear si el nombre ya está declarado
    mty <- lookupTy n
    case mty of
        Nothing -> do  --no está declarado 
                  s <- get
                  tt <- tc t (tyEnv s) (types s)
                  return (Decl p n tt)
        Just _  -> failPosFD4 p $ n ++" ya está declarado"
tcDecl (TyDecl p n t) = do
    --chequear si el nombre ya está declarado
    mty <- lookupTypes n
    case mty of
        Nothing -> do  --no está declarado 
                  s <- get
                  tt <- tcty p t (types s)
                  return (TyDecl p n tt)
        Just _  -> failPosFD4 p $ "tipo " ++ n ++" ya está declarado"

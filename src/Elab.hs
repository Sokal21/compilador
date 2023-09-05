-- |
-- Module      : Elab
-- Description : Elabora un término fully named a uno locally closed.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
--
-- Este módulo permite elaborar términos y declaraciones para convertirlas desde
-- fully named (@STerm) a locally closed (@Term@)
module Elab (elab, elabDecl) where

import Lang
import MonadFD4 (MonadFD4)
import Subst

elabTy :: STy -> Ty
elabTy SNatTy = NatTy
elabTy (SFunTy a e) = FunTy (elabTy a) (elabTy e)
elabTy (SDeclTy a) = DeclTy a

-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado.
elab :: MonadFD4 m => STerm -> m Term
elab = elab' []

elab' :: MonadFD4 m => [Name] -> STerm -> m Term
elab' env (SV p v) =
  -- Tenemos que ver si la variable es Global o es un nombre local
  -- En env llevamos la lista de nombres locales.
  if v `elem` env
    then return $ V p (Free v)
    else return $ V p (Global v)
elab' _ (SConst p c) = return $ Const p c
elab' env (SLam p [] t) = error "try to elab no params lam"
elab' env (SLam p [(v, sty)] t) = do
  e <- elab' (v : env) t
  return $ Lam p v (elabTy sty) (close v e)
elab' env (SLam p ((v, sty) : args) t) = do
  e <- elab' (v : env) (SLam p args t)
  return $ Lam p v (elabTy sty) (close v e)
elab' env (SFix p (f, sfty) [] t) = error "try to elab no params fix"
elab' env (SFix p (f, sfty) [(x, sxty)] t) = do
  e <- elab' (x : f : env) t
  return $ Fix p f (elabTy sfty) x (elabTy sxty) (close2 f x e)
elab' env (SFix p (f, sfty) ((x, sxty) : args) t) = do
  e <- elab' (x : f : env) $ SLam p args t
  return $ Fix p f (elabTy sfty) x (elabTy sxty) (close2 f x e)
elab' env (SIfZ p c t e) = do
  b <- elab' env c
  l <- elab' env t
  r <- elab' env e
  return $ IfZ p b l r
-- Operadores binarios
elab' env (SBinaryOp i o t u) = do
  l <- elab' env t
  r <- elab' env u
  return $ BinaryOp i o l r
-- Operador Print
elab' env (SPrint i str t) = do
  e <- elab' env t
  return $ Print i str e
-- Aplicaciones generales
elab' env (SApp p h a) = do
  l <- elab' env h
  r <- elab' env a
  return $ App p l r
elab' env (SLet p (v, vty) def body) = do
  d <- elab' env def
  b <- elab' (v : env) body
  return $ Let p v (elabTy vty) d (close v b)
elab' env (SLetLam p (f, fty) args d b No) =
  elab' env $ SLet p (f, fty) (SLam p args d) b
elab' env (SLetLam p (f, fty) args d b Yes) =
  elab' env $ SLet p (f, fty) (SFix p (f, fty) args d) b

elabDecl :: MonadFD4 m => SDecl STerm -> m (Decl Term)
elabDecl (SDecl p n sty sb) = do
  b <- elab sb
  return $ Decl p n b
elabDecl (SDeclType p n sty) = do
  return $ TyDecl p n (elabTy sty)
elabDecl (SDeclLam p n args ty b No) =
  elabDecl (SDecl p n ty (SLam p args b))
elabDecl (SDeclLam p n args ty b Yes) =
  elabDecl (SDecl p n ty (SFix p (n, ty) args b))

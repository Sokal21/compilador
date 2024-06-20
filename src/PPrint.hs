{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use record patterns" #-}

-- |
-- Module      : PPrint
-- Description : Pretty printer para FD4.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
module PPrint
  ( pp,
    ppTy,
    ppName,
    ppDecl,
    freshSTy,
  )
where

import Common (Pos)
import Data.Text (unpack)
import Global (GlEnv (glb))
import Lang
import MonadFD4 (MonadFD4, gets)
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    annotate,
    defaultLayoutOptions,
    layoutSmart,
    nest,
    parens,
    sep,
    (<+>),
  )
import Prettyprinter.Render.Terminal
  ( AnsiStyle,
    Color (..),
    color,
    colorDull,
    italicized,
    renderStrict,
  )
import Subst (open, open2)

freshSTy :: Ty -> STy
freshSTy NatTy = SNatTy
freshSTy (FunTy a e) = SFunTy (freshSTy a) (freshSTy e)
freshSTy (DeclTy a) = SDeclTy a

freshen :: [Name] -> Name -> Name
freshen ns n =
  let cands = n : map (\i -> n ++ show i) [0 ..]
   in head (filter (`notElem` ns) cands)

-- | 'openAll' convierte términos locally nameless
-- a términos fully named abriendo todos las variables de ligadura que va encontrando
-- Debe tener cuidado de no abrir términos con nombres que ya fueron abiertos.
-- Estos nombres se encuentran en la lista ns (primer argumento).
openAll :: (i -> Pos) -> [Name] -> Tm i Var -> STerm
openAll gp ns (V p v) = case v of
  Bound i -> SV (gp p) $ "(Bound " ++ show i ++ ")" -- este caso no debería aparecer
  -- si el término es localmente cerrado
  Free x -> SV (gp p) x
  Global x -> SV (gp p) x
openAll gp ns (Const p c) = SConst (gp p) c
openAll gp ns (Lam p x ty t) =
  let x' = freshen ns x
   in SLam (gp p) [([x'], freshSTy ty)] (openAll gp (x' : ns) (open x' t))
openAll gp ns (App p t u) = SApp (gp p) (openAll gp ns t) (openAll gp ns u)
openAll gp ns (Fix p f fty x xty t) =
  let x' = freshen ns x
      f' = freshen (x' : ns) f
   in SFix (gp p) (f', freshSTy fty) [([x'], freshSTy xty)] (openAll gp (x : f : ns) (open2 f' x' t))
openAll gp ns (IfZ p c t e) = SIfZ (gp p) (openAll gp ns c) (openAll gp ns t) (openAll gp ns e)
openAll gp ns (Print p str t) = SPrint (gp p) str (openAll gp ns t)
openAll gp ns (BinaryOp p op t u) = SBinaryOp (gp p) op (openAll gp ns t) (openAll gp ns u)
openAll gp ns (Let p v ty m n) =
  let v' = freshen ns v
   in SLet (gp p) (v', freshSTy ty) (openAll gp ns m) (openAll gp (v' : ns) (open v' n))

-- Colores
constColor :: Doc AnsiStyle -> Doc AnsiStyle
constColor = annotate (color Red)

opColor :: Doc AnsiStyle -> Doc AnsiStyle
opColor = annotate (colorDull Green)

keywordColor :: Doc AnsiStyle -> Doc AnsiStyle
keywordColor = annotate (colorDull Green) -- <> bold)

typeColor :: Doc AnsiStyle -> Doc AnsiStyle
typeColor = annotate (color Blue <> italicized)

typeOpColor :: Doc AnsiStyle -> Doc AnsiStyle
typeOpColor = annotate (colorDull Blue)

defColor :: Doc AnsiStyle -> Doc AnsiStyle
defColor = annotate (colorDull Magenta <> italicized)

nameColor :: Doc AnsiStyle -> Doc AnsiStyle
nameColor = id

-- | Pretty printer de nombres (Doc)
name2doc :: Name -> Doc AnsiStyle
name2doc n = nameColor (pretty n)

-- |  Pretty printer de nombres (String)
ppName :: Name -> String
ppName = id

-- | Pretty printer para tipos (Doc)
ty2doc :: STy -> Doc AnsiStyle
ty2doc SNatTy = typeColor (pretty "Nat")
ty2doc (SFunTy x@(SFunTy _ _) y) = sep [parens (ty2doc x), typeOpColor (pretty "->"), ty2doc y]
ty2doc (SFunTy x y) = sep [ty2doc x, typeOpColor (pretty "->"), ty2doc y]
ty2doc (SDeclTy n) = typeColor (pretty n)

-- | Pretty printer para tipos (String)
ppTy :: STy -> String
ppTy = render . ty2doc

c2doc :: Const -> Doc AnsiStyle
c2doc (CNat n) = constColor (pretty (show n))

binary2doc :: BinaryOp -> Doc AnsiStyle
binary2doc Add = opColor (pretty "+")
binary2doc Sub = opColor (pretty "-")

collectApp :: STerm -> (STerm, [STerm])
collectApp = go []
  where
    go ts (SApp _ h tt) = go (tt : ts) h
    go ts h = (h, ts)

parenIf :: Bool -> Doc a -> Doc a
parenIf True = parens
parenIf _ = id

-- t2doc at t :: Doc
-- at: debe ser un átomo

-- | Pretty printing de términos (Doc)
t2doc ::
  Bool -> -- Debe ser un átomo?
  STerm -> -- término a mostrar
  Doc AnsiStyle
-- Uncomment to use the Show instance for STerm
{- t2doc at x = text (show x) -}
t2doc at (SLetLam i (f, ty) args def body r) =
  parenIf at $
    sep
      [ sep
          [ keywordColor
              ( pretty
                  ( case r of
                      Yes -> "let rec"
                      No -> "let"
                  )
              ),
            name2doc f,
            mbinding2doc args,
            pretty ":",
            ty2doc ty,
            opColor (pretty "=")
          ],
        nest 2 (t2doc False def),
        keywordColor (pretty "in"),
        nest 2 (t2doc False body)
      ]
t2doc at (SV _ x) = name2doc x
t2doc at (SConst _ c) = c2doc c
t2doc at (SLam _ args t) =
  parenIf at $
    sep
      [ sep
          [ keywordColor (pretty "fun"),
            mbinding2doc args,
            opColor (pretty "->")
          ],
        nest 2 (t2doc False t)
      ]
t2doc at t@(SApp _ _ _) =
  let (h, ts) = collectApp t
   in parenIf at $
        t2doc True h <+> sep (map (t2doc True) ts)
t2doc at (SFix _ f args m) =
  parenIf at $
    sep
      [ sep
          [ keywordColor (pretty "fix"),
            binding2doc [f],
            mbinding2doc args,
            opColor (pretty "->")
          ],
        nest 2 (t2doc False m)
      ]
t2doc at (SIfZ _ c t e) =
  parenIf at $
    sep
      [ keywordColor (pretty "ifz"),
        nest 2 (t2doc False c),
        keywordColor (pretty "then"),
        nest 2 (t2doc False t),
        keywordColor (pretty "else"),
        nest 2 (t2doc False e)
      ]
t2doc at (SPrint _ str t) =
  parenIf at $
    sep [keywordColor (pretty "print"), pretty (show str), t2doc True t]
t2doc at (SLet _ v t t') =
  parenIf at $
    sep
      [ sep
          [ keywordColor (pretty "let"),
            binding2doc [v],
            opColor (pretty "=")
          ],
        nest 2 (t2doc False t),
        keywordColor (pretty "in"),
        nest 2 (t2doc False t')
      ]
t2doc at (SBinaryOp _ o a b) =
  parenIf at $
    t2doc True a <+> binary2doc o <+> t2doc True b

mbinding2doc :: [Binding Name STy] -> Doc AnsiStyle
mbinding2doc [] = pretty ""
mbinding2doc [(x, ty)] =
  parens (sep (map name2doc x ++ [pretty ":", ty2doc ty]))
mbinding2doc ((x, ty) : xs) =
  parens (sep (map name2doc x ++ [pretty ":", ty2doc ty])) <+> mbinding2doc xs

binding2doc :: [(Name, STy)] -> Doc AnsiStyle
binding2doc [] = pretty ""
binding2doc [(x, ty)] =
  parens (sep [name2doc x, pretty ":", ty2doc ty])
binding2doc ((x, ty) : xs) =
  parens (sep [name2doc x, pretty ":", ty2doc ty]) <+> binding2doc xs

-- | Pretty printing de términos (String)
pp :: (MonadFD4 m) => TTerm -> m String
-- Uncomment to use the Show instance for Term
{- pp = show -}

recResugarTerm :: ResugarerTerm -> STerm -> (Bool, STerm)
recResugarTerm f st@(SV _ _) = (False, st)
recResugarTerm f st@(SConst _ _) = (False, st)
recResugarTerm f (SLam i args t) = let (b, rt) = f t in (b, SLam i args rt)
recResugarTerm f (SApp i lt rt) =
  let (bl, rlt) = f lt
      (br, rrt) = f rt
   in (bl || br, SApp i rlt rrt)
recResugarTerm f (SPrint i s t) = let (b, rt) = f t in (b, SPrint i s rt)
recResugarTerm f (SBinaryOp i o lt rt) =
  let (bl, rlt) = f lt
      (br, rrt) = f rt
   in (bl || br, SBinaryOp i o rlt rrt)
recResugarTerm f (SFix i v args t) = let (b, rt) = f t in (b, SFix i v args rt)
recResugarTerm f (SIfZ i ct tt et) =
  let (bc, rct) = f ct
      (bt, rtt) = f tt
      (be, ret) = f et
   in (bc || bt || be, SIfZ i ct tt et)
recResugarTerm f (SLet i v lt rt) =
  let (bl, rlt) = f lt
      (br, rrt) = f rt
   in (bl || br, SLet i v rlt rrt)
recResugarTerm f (SLetLam i v args lt rt r) =
  let (bl, rlt) = f lt
      (br, rrt) = f rt
   in (bl || br, SLetLam i v args rlt rrt r)

rsugarFun :: ResugarerTerm
rsugarFun (SLam i fa (SLam _ sa t)) = (True, SLam i (fa ++ sa) (snd $ rsugarFun t))
rsugarFun x = recResugarTerm rsugarFun x

rsugarFix :: ResugarerTerm
rsugarFix (SFix i x fa (SLam _ sa t)) = (True, SFix i x (fa ++ sa) (snd $ rsugarFix t))
rsugarFix x = recResugarTerm rsugarFix x

rsugarLetLam :: ResugarerTerm
rsugarLetLam (SLet i v (SLam _ args ft) rt) = (True, SLetLam i v args (snd $ rsugarLetLam ft) (snd $ rsugarLetLam rt) No)
rsugarLetLam (SLet i _ (SFix _ vs args t) rt) = (True, SLetLam i vs args (snd $ rsugarLetLam t) (snd $ rsugarLetLam rt) Yes)
rsugarLetLam x = recResugarTerm rsugarLetLam x

combineArgs :: [Binding Name STy] -> (Bool, [Binding Name STy])
combineArgs [] = (False, [])
combineArgs [x] = (False, [x])
combineArgs ((x, xty) : (y, yty) : args)
  | xty == yty = (True, (x ++ y, xty) : snd (combineArgs args))
  | otherwise = let (b, cargs) = combineArgs ((y, yty) : args) in (b, (x, xty) : cargs)

rsugarMultBind :: ResugarerTerm
rsugarMultBind (SLam i x t) =
  let (b, cx) = combineArgs x
      (bt, rt) = rsugarMultBind t
   in (b || bt, SLam i cx rt)
rsugarMultBind (SFix i x args t) =
  let (b, cargs) = combineArgs args
      (bt, rt) = rsugarMultBind t
   in (b || bt, SFix i x cargs rt)
rsugarMultBind (SLetLam i x args lt rt r) =
  let (b, cargs) = combineArgs args
      (bl, clt) = rsugarMultBind lt
      (br, crt) = rsugarMultBind rt
   in (b || bl || br, SLetLam i x cargs clt crt r)
rsugarMultBind ft = recResugarTerm rsugarMultBind ft

type ResugarerTerm = STerm -> (Bool, STerm)

rsugarTermRec :: [ResugarerTerm] -> Bool -> STerm -> STerm
rsugarTermRec ops c t
  | c =
      let ott = foldr (\op (cond, tt) -> let (c', t') = op tt in (cond || c', t')) (False, t) ops
       in uncurry (rsugarTermRec ops) ott
  | otherwise = t

resugarerTerm :: STerm -> STerm
resugarerTerm = rsugarTermRec [rsugarFun, rsugarFix, rsugarLetLam, rsugarMultBind] True

type ResugarerDecl = SDecl STerm -> (Bool, SDecl STerm)

rsugarDeclLam :: ResugarerDecl
rsugarDeclLam (SDecl p n ty (SLam _ args v)) = (True, SDeclLam p n args ty v No)
rsugarDeclLam (SDecl p n ty (SFix _ _ args v)) = (True, SDeclLam p n args ty v Yes)
rsugarDeclLam x = (False, x)

rsugarDeclFun :: ResugarerDecl
rsugarDeclFun (SDeclLam p n args t (SLam _ bargs v) r) = (True, SDeclLam p n (args ++ bargs) t v r)
rsugarDeclFun x = (False, x)

rsugarDeclMulti :: ResugarerDecl
rsugarDeclMulti (SDeclLam p n args t b r) = let (cb, cargs) = combineArgs args in (cb, SDeclLam p n cargs t b r)
rsugarDeclMulti x = (False, x)

applyResugarDecl :: SDecl STerm -> SDecl STerm
applyResugarDecl (SDecl p n t b) = SDecl p n t (resugarerTerm b)
applyResugarDecl (SDeclLam p n args t b r) = SDeclLam p n args t (resugarerTerm b) r
applyResugarDecl x = x

rsugarDeclRec :: [ResugarerDecl] -> Bool -> SDecl STerm -> SDecl STerm
rsugarDeclRec ops c t
  | c =
      let ott = foldr (\op (cond, tt) -> let (c', t') = op tt in (cond || c', t')) (False, applyResugarDecl t) ops
       in uncurry (rsugarDeclRec ops) ott
  | otherwise = t

resugarerDecl :: SDecl STerm -> SDecl STerm
resugarerDecl = rsugarDeclRec [rsugarDeclLam, rsugarDeclFun, rsugarDeclMulti] True

pp t = do
  gdecl <- gets glb
  return (render . t2doc False $ resugarerTerm $ openAll fst (map declName gdecl) t)

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

dt2sdt :: [Decl TTerm] -> Decl TTerm -> SDecl STerm
dt2sdt _ (TyDecl p n t) = SDeclType p n (freshSTy t)
dt2sdt gdecl (Decl p n b) = SDecl p n (freshSTy (getTy b)) (openAll fst (map declName gdecl) b)

ppDecl' :: (MonadFD4 m) => SDecl STerm -> m String
ppDecl' (SDeclType p n t) = do
  return
    ( render $
        sep
          [ defColor (pretty "type"),
            name2doc n,
            defColor (pretty "=")
          ]
          <+> nest 2 (ty2doc t)
    )
ppDecl' (SDecl p n t b) = do
  return
    ( render $
        sep
          [ defColor (pretty "let"),
            name2doc n,
            defColor (pretty ":"),
            ty2doc t,
            defColor (pretty "=")
          ]
          <+> nest 2 (t2doc False (resugarerTerm b))
    )
ppDecl' (SDeclLam p n args t b r) = do
  return
    ( render $
        sep
          ( (if match r then [defColor (pretty "let rec")] else [defColor (pretty "let")])
              ++ [ name2doc n,
                   defColor (pretty ":"),
                   ty2doc t,
                   defColor (pretty "=")
                 ]
          )
          <+> nest 2 (t2doc False (resugarerTerm b))
    )
  where
    match Yes = True
    match _ = False

-- | Pretty printing de declaraciones
ppDecl :: (MonadFD4 m) => Decl TTerm -> m String
ppDecl dec = do
  gdecl <- gets glb
  let sdec = resugarerDecl (dt2sdt gdecl dec)
  ppDecl' sdec

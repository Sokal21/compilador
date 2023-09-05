module CombinedModule where

import Parse
import Elab
import Lang
import MonadFD4 (FD4, getTyEnv, addTy, runFD4)
import Global


import Debug.Trace
-- h = elabDecl' [] $ runP decl "let x: Nat = 0" ""

debug :: String -> FD4 (Decl Term)
debug s = case runP decl s "" of
  Right t -> do 
    x <- elabDecl' [] t
    trace (show x) $ return x
  Left e -> error ("no parse: " ++ show s)
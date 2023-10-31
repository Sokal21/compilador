{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Bytecompile
-- Description : Compila a bytecode. Ejecuta bytecode.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
--
-- Este módulo permite compilar módulos a la Macchina. También provee
-- una implementación de la Macchina para ejecutar el bytecode.
module Bytecompile (Bytecode, runBC, bcWrite, bcRead, bytecompileModule, showBC, fileExtesion) where

import Common (lookUpIndex)
import Data.Binary (Binary (get, put), Word32, decode, encode)
import Data.Binary.Get (getWord32le, isEmpty)
import Data.Binary.Put (putWord32le)
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.List (intercalate)
import Lang
import MonadFD4
import Subst

fileExtesion :: String
fileExtesion = ".bc32"

type Opcode = Int

type Bytecode = [Int]

newtype Bytecode32 = BC {un32 :: [Word32]}

{- Esta instancia explica como codificar y decodificar Bytecode de 32 bits -}
instance Binary Bytecode32 where
  put (BC bs) = mapM_ putWord32le bs
  get = go
    where
      go =
        do
          empty <- isEmpty
          if empty
            then return $ BC []
            else do
              x <- getWord32le
              BC xs <- go
              return $ BC (x : xs)

{- Estos sinónimos de patrón nos permiten escribir y hacer
pattern-matching sobre el nombre de la operación en lugar del código
entero, por ejemplo:

   f (CALL : cs) = ...

 Notar que si hubieramos escrito algo como
   call = 5
 no podríamos hacer pattern-matching con `call`.

 En lo posible, usar estos códigos exactos para poder ejectutar un
 mismo bytecode compilado en distintas implementaciones de la máquina.
-}
pattern NULL = 0

pattern RETURN = 1

pattern CONST = 2

pattern ACCESS = 3

pattern FUNCTION = 4

pattern CALL = 5

pattern ADD = 6

pattern SUB = 7

pattern IFZ = 8

pattern FIX = 9

pattern STOP = 10

pattern SHIFT = 11

pattern DROP = 12

pattern PRINT = 13

pattern PRINTN = 14

pattern JUMP = 15

pattern TAILCALL = 16

-- función util para debugging: muestra el Bytecode de forma más legible.
showOps :: Bytecode -> [String]
showOps [] = []
showOps (NULL : xs) = "NULL" : showOps xs
showOps (RETURN : xs) = "RETURN" : showOps xs
showOps (CONST : i : xs) = ("CONST " ++ show i) : showOps xs
showOps (ACCESS : i : xs) = ("ACCESS " ++ show i) : showOps xs
showOps (FUNCTION : i : xs) = ("FUNCTION len=" ++ show i) : showOps xs
showOps (CALL : xs) = "CALL" : showOps xs
showOps (ADD : xs) = "ADD" : showOps xs
showOps (SUB : xs) = "SUB" : showOps xs
showOps (FIX : xs) = "FIX" : showOps xs
showOps (IFZ : i : xs) = ("IFZ off =" ++ show i) : showOps xs
showOps (STOP : xs) = "STOP" : showOps xs
showOps (JUMP : i : xs) = ("JUMP off=" ++ show i) : showOps xs
showOps (SHIFT : xs) = "SHIFT" : showOps xs
showOps (DROP : xs) = "DROP" : showOps xs
showOps (PRINT : xs) =
  let (msg, _ : rest) = span (/= NULL) xs
   in ("PRINT " ++ show (bc2string msg)) : showOps rest
showOps (PRINTN : xs) = "PRINTN" : showOps xs
showOps (ADD : xs) = "ADD" : showOps xs
showOps (TAILCALL : xs) = "TAILCALL" : showOps xs
showOps (x : xs) = show x : showOps xs

showBC :: Bytecode -> String
showBC = intercalate "; " . showOps

opToBcc :: BinaryOp -> Bytecode
opToBcc Add = [ADD]
opToBcc Sub = [SUB]

bcTailcall :: (MonadFD4 m) => TTerm -> m Bytecode
bcTailcall (App _ l r) = do
  bcl <- bcDrop l
  bcr <- bcc r
  return $ bcl ++ bcr ++ [TAILCALL]
bcTailcall (IfZ _ c t e) = do
  bc <- bcDrop c
  dt <- bcTailcall t
  de <- bcTailcall e
  return $ bc ++ [IFZ, length dt + 2] ++ dt ++ [JUMP, length de] ++ de
bcTailcall (Let _ _ _ tt (Sc1 dt)) = do
  bctt <- bcDrop tt
  bcdt <- bcTailcall dt
  return $ bctt ++ [SHIFT] ++ bcdt
bcTailcall xs = do
  bxs <- bcDrop xs
  return $ bxs ++ [RETURN]

bcc :: (MonadFD4 m) => TTerm -> m Bytecode
bcc (V _ (Bound num)) = return [ACCESS, num]
bcc (V _ _) = error "No podes entrar aca, papu"
bcc (Const _ (CNat num)) = return [CONST, num]
bcc (BinaryOp _ op lt rt) = do
  bcl <- bcDrop lt
  bcr <- bcc rt
  return $ bcl ++ bcr ++ opToBcc op
bcc (Print _ str tt) = do
  bc <- bcc tt
  return $ bc ++ [PRINT] ++ string2bc str ++ [NULL] ++ [PRINTN]
bcc (App _ ft vt) = do
  bcf <- bcDrop ft
  bcv <- bcc vt
  return $ bcf ++ bcv ++ [CALL]
bcc (Lam _ _ _ (Sc1 tt)) = do
  bctt <- bcTailcall tt
  return $ [FUNCTION] ++ [length bctt] ++ bctt
bcc (Fix _ _ _ _ _ (Sc2 bt)) = do
  bcbt <- bcTailcall bt
  return $ [FUNCTION] ++ [length bcbt] ++ bcbt ++ [FIX]
bcc (Let _ _ _ tt (Sc1 dt)) = do
  bctt <- bcDrop tt
  bcdt <- bcc dt
  return $ bctt ++ [SHIFT] ++ bcdt
bcc (IfZ _ ct tt et) = do
  bcct <- bcDrop ct
  bctt <- bcc tt
  bcet <- bcc et
  return $ bcct ++ [IFZ, length bctt + 2] ++ bctt ++ [JUMP, length bcet] ++ bcet

bcDrop :: (MonadFD4 m) => TTerm -> m Bytecode
bcDrop (Let _ _ _ tt (Sc1 dt)) = do
  bctt <- bcDrop tt
  bcdt <- bcc dt
  return $ bctt ++ [SHIFT] ++ bcdt ++ [DROP]
bcDrop bc = bcc bc

-- ord/chr devuelven los codepoints unicode, o en otras palabras
-- la codificación UTF-32 del caracter.
string2bc :: String -> Bytecode
string2bc = map ord

bc2string :: Bytecode -> String
bc2string = map chr

skipTyDecl :: Module -> Maybe Module
skipTyDecl [] = Nothing
skipTyDecl ((TyDecl {}) : xs) = skipTyDecl xs
skipTyDecl x@((Decl {} : xs)) = Just x

removeGlobals :: TTerm -> TTerm
removeGlobals (V p (Global n)) = V p (Free n)
removeGlobals (Lam i x xty (Sc1 t)) = Lam i x xty (Sc1 (removeGlobals t))
removeGlobals (App i l r) = App i (removeGlobals l) (removeGlobals r)
removeGlobals (Print i s t) = Print i s (removeGlobals t)
removeGlobals (BinaryOp i op l r) = BinaryOp i op (removeGlobals l) (removeGlobals r)
removeGlobals (Fix i n ty n' ty' (Sc2 t)) = Fix i n ty n' ty' (Sc2 (removeGlobals t))
removeGlobals (IfZ i c t e) = IfZ i (removeGlobals c) (removeGlobals t) (removeGlobals e)
removeGlobals (Let i n ty l (Sc1 s)) = Let i n ty (removeGlobals l) (Sc1 (removeGlobals s))
removeGlobals t = t

translate :: (MonadFD4 m) => Module -> m TTerm
translate ((Decl p n b) : ds) = case skipTyDecl ds of
  Nothing -> return $ removeGlobals b
  Just d -> do
    tx <- translate d
    return $ Let (p, getTy b) n (getTy b) (removeGlobals b) (close n tx)
translate ds = case skipTyDecl ds of
  Nothing -> error "Modulo no valido"
  Just d -> translate d

bytecompileModule :: (MonadFD4 m) => Module -> m Bytecode
bytecompileModule m = do
  t <- translate m
  bc <- bcc t
  return $ bc ++ [STOP]

-- | Toma un bytecode, lo codifica y lo escribe un archivo
bcWrite :: Bytecode -> FilePath -> IO ()
bcWrite bs filename = BS.writeFile filename (encode $ BC $ fromIntegral <$> bs)

---------------------------

-- * Ejecución de bytecode

---------------------------

-- | Lee de un archivo y lo decodifica a bytecode
bcRead :: FilePath -> IO Bytecode
bcRead filename = (map fromIntegral <$> un32) . decode <$> BS.readFile filename

type Env = [Val]

type Stack = [Val]

data Val = I Int | Fun Env Bytecode | RA Env Bytecode deriving (Show)

eFix :: Bytecode -> Env -> Env
eFix cf e = Fun (eFix cf e) cf : e

profileStack :: (MonadFD4 m) => Stack -> m Stack
profileStack s = do
  addMaxStack $ length s
  return s

evalBC :: (MonadFD4 m) => Bytecode -> Env -> Stack -> m Int
evalBC (STOP : bc) _ _ = return 0
evalBC (CONST : n : bc) e s = addOpp >> profileStack (I n : s) >>= evalBC bc e
evalBC (ADD : bc) e (I l : I r : s) = 
  addOpp >> profileStack (I (l + r) : s) >>= evalBC bc e
evalBC (SUB : bc) e (I l : I r : s) = addOpp >> profileStack (I (r - l) : s) >>= evalBC bc e
evalBC (ACCESS : i : bc) e s = case lookUpIndex i e of
  Nothing -> do
    error "No pudimos indexar la variable, papu"
  Just n -> addOpp >> profileStack (n : s) >>= evalBC bc e
evalBC (CALL : bc) e (v : Fun ef bcf : s) = 
  addOpp >> addClos >> profileStack (RA e bc : s) >>= evalBC bcf (v : ef)
evalBC (FUNCTION : bl : bc) e s = 
  addOpp >> addClos >> profileStack (Fun e (take bl bc) : s) >>= evalBC (drop bl bc) e
evalBC (RETURN : _) _ (v : (RA re rbc) : s) = 
  addOpp >> profileStack (v : s) >>= evalBC rbc re
evalBC (SHIFT : bc) e (v : s) = addOpp >> evalBC bc (v : e) s
evalBC (DROP : bc) (v : e) s = addOpp >> evalBC bc e s
evalBC (PRINTN : bc) e st@((I k) : s) = do
  addOpp
  printFD4 $ show k
  evalBC bc e st
evalBC (PRINT : bc) e s = do
  addOpp
  printStr $ bc2string (takeWhile (/= NULL) bc)
  evalBC (tail (dropWhile (/= NULL) bc)) e s
evalBC (FIX : bc) e ((Fun fe fb) : s) = 
  addOpp >> addClos >> profileStack (Fun (eFix fb fe) fb : s) >>= evalBC bc e
evalBC (IFZ : tl : bc) e ((I v) : s)
  | v == 0 = addOpp >> evalBC bc e s
  | otherwise = addOpp >> evalBC (drop tl bc) e s
evalBC (JUMP : n : bc) e s = addOpp >> evalBC (drop n bc) e s
evalBC (TAILCALL : bc) e (v : Fun ef bcf : s) = addOpp >> evalBC bcf (v : ef) s
evalBC bc e s = error "El programa es invalido, papu"

runBC :: (MonadFD4 m) => Bytecode -> m ()
runBC bc = do
  t <- evalBC bc [] []
  return ()
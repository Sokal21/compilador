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
module Bytecompile8b (Bytecode, runBC, bcWrite, bcRead, bytecompileModule, showBC, fileExtesion) where

import Common (lookUpIndex)
import Data.Binary (Binary (get, put), Word8, decode, encode, getWord8)
import Data.Binary.Get (isEmpty)
import Data.Binary.Put (putWord8)
import Data.Bits
import qualified Data.ByteString.Lazy as BS
import Data.Char
import Data.List (intercalate)
import Lang
import MonadFD4
import Subst

fileExtesion :: String
fileExtesion = ".bc8"

type Opcode = Word8

type Bytecode = [Word8]

newtype Bytecode8 = BC {un8 :: [Word8]}

{- Esta instancia explica como codificar y decodificar Bytecode de 32 bits -}
instance Binary Bytecode8 where
  put (BC bs) = mapM_ putWord8 bs
  get = go
    where
      go =
        do
          empty <- isEmpty
          if empty
            then return $ BC []
            else do
              x <- getWord8
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

pattern BYTE = 16

pattern WORD = 17

pattern LONG = 18

decompose32 :: Int -> Bytecode
decompose32 n =
  let b1 = fromIntegral (n `shiftR` 24)
      b2 = fromIntegral ((n `shiftR` 16) .&. 0xFF)
      b3 = fromIntegral ((n `shiftR` 8) .&. 0xFF)
      b4 = fromIntegral (n .&. 0xFF)
   in assemble b1 b2 b3 b4
  where
    assemble 0 0 0 b4 = [BYTE, b4]
    assemble 0 0 b3 b4 = [WORD, b3, b4]
    assemble b1 b2 b3 b4 = [LONG, b1, b2, b3, b4]

recompose32 :: Word8 -> Word8 -> Word8 -> Word8 -> Int
recompose32 a b c d =
  let a' = fromIntegral a `shiftL` 24
      b' = fromIntegral b `shiftL` 16
      c' = fromIntegral c `shiftL` 8
      d' = fromIntegral d
   in a' + b' + c' + d'

-- función util para debugging: muestra el Bytecode de forma más legible.
showOps :: Bytecode -> [String]
showOps [] = []
showOps (NULL : xs) = "NULL" : showOps xs
showOps (RETURN : xs) = "RETURN" : showOps xs
showOps (BYTE : i : xs) = ("BYTE " ++ show (recompose32 0 0 0 i)) : showOps xs
showOps (WORD : i : j : xs) = ("WORD " ++ show (recompose32 0 0 i j)) : showOps xs
showOps (LONG : k : l : i : j : xs) = ("LONG " ++ show (recompose32 k l i j)) : showOps xs
showOps (ACCESS : i : xs) = ("ACCESS " ++ show i) : showOps xs
showOps (FUNCTION : i : xs) = ("FUNCTION len=" ++ show i) : showOps xs
showOps (CALL : xs) = "CALL" : showOps xs
showOps (ADD : xs) = "ADD" : showOps xs
showOps (SUB : xs) = "SUB" : showOps xs
showOps (FIX : xs) = "FIX" : showOps xs
showOps (STOP : xs) = "STOP" : showOps xs
showOps (JUMP : i : xs) = ("JUMP off=" ++ show i) : showOps xs
showOps (SHIFT : xs) = "SHIFT" : showOps xs
showOps (DROP : xs) = "DROP" : showOps xs
showOps (PRINT : xs) =
  let (msg, _ : rest) = span (/= NULL) xs
   in ("PRINT " ++ show (bc2string msg)) : showOps rest
showOps (PRINTN : xs) = "PRINTN" : showOps xs
showOps (ADD : xs) = "ADD" : showOps xs
showOps (x : xs) = show x : showOps xs

showBC :: Bytecode -> String
showBC = intercalate "; " . showOps

opToBcc :: BinaryOp -> Bytecode
opToBcc Add = [ADD]
opToBcc Sub = [SUB]

bcc :: (MonadFD4 m) => TTerm -> m Bytecode
bcc (V _ (Bound num)) = return $ ACCESS : decompose32 num
bcc (V _ _) = error "No podes entrar aca, papu"
bcc (Const _ (CNat num)) = return $ decompose32 num
bcc (BinaryOp _ op lt rt) = do
  bcl <- bcc lt
  bcr <- bcc rt
  return $ bcl ++ bcr ++ opToBcc op
bcc (Print _ str tt) = do
  bc <- bcc tt
  return $ bc ++ [PRINT] ++ string2bc str ++ [NULL] ++ [PRINTN]
bcc (App _ ft vt) = do
  bcf <- bcc ft
  bcv <- bcc vt
  return $ bcf ++ bcv ++ [CALL]
bcc (Lam _ _ _ (Sc1 tt)) = do
  bctt <- bcc tt
  return $ [FUNCTION] ++ decompose32 (length bctt + 1) ++ bctt ++ [RETURN]
bcc (Fix _ _ _ _ _ (Sc2 bt)) = do
  bcbt <- bcc bt
  return $ [FUNCTION] ++ decompose32 (length bcbt + 1) ++ bcbt ++ [RETURN, FIX]
bcc (Let _ _ _ tt (Sc1 dt)) = do
  bctt <- bcc tt
  bcdt <- bcc dt
  return $ bctt ++ [SHIFT] ++ bcdt ++ [DROP]
bcc (IfZ _ ct tt et) = do
  bcct <- bcc ct
  bctt <- bcc tt
  bcet <- bcc et
  return $ bcct ++ [IFZ] ++ decompose32 (length bctt + 2) ++ bctt ++ [JUMP] ++ decompose32 (length bcet) ++ bcet

-- ord/chr devuelven los codepoints unicode, o en otras palabras
-- la codificación UTF-32 del caracter.
string2bc :: String -> Bytecode
string2bc = map (fromIntegral . ord)

bc2string :: Bytecode -> String
bc2string = map (chr . fromIntegral)

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
bcRead filename = (map fromIntegral <$> un8) . decode <$> BS.readFile filename

type Env = [Val]

type Stack = [Val]

data Val = I Int | Fun Env Bytecode | RA Env Bytecode

eFix :: Bytecode -> Env -> Env
eFix cf e = Fun (eFix cf e) cf : e

readNumber :: Bytecode -> (Int, Bytecode)
readNumber (BYTE : n : bc) = (recompose32 0 0 0 n, bc)
readNumber (WORD : n : m : bc) = (recompose32 0 0 n m, bc)
readNumber (LONG : k : l : n : m : bc) = (recompose32 k l n m, bc)

evalBC :: (MonadFD4 m) => Bytecode -> Env -> Stack -> m Int
evalBC (STOP : bc) e ((I r) : s) = return r
-- evalBC (CONST : n : bc) e s = evalBC bc e (I n : s)
evalBC bc@(BYTE : _) e s = evalBC (snd (readNumber bc)) e (I (fst (readNumber bc)) : s)
evalBC bc@(WORD : _) e s = evalBC (snd (readNumber bc)) e (I (fst (readNumber bc)) : s)
evalBC bc@(LONG : _) e s = evalBC (snd (readNumber bc)) e (I (fst (readNumber bc)) : s)
evalBC (ADD : bc) e (I l : I r : s) = evalBC bc e (I (l + r) : s)
evalBC (SUB : bc) e (I l : I r : s) = evalBC bc e (I (r - l) : s)
evalBC (ACCESS : bc) e s = case lookUpIndex (fst (readNumber bc)) e of
  Nothing -> error "No pudimos indexar la variable, papu"
  Just n -> evalBC (snd (readNumber bc)) e (n : s)
evalBC (CALL : bc) e (v : Fun ef bcf : s) = evalBC bcf (v : ef) (RA e bc : s)
evalBC (FUNCTION : bc) e s = evalBC (uncurry drop (readNumber bc)) e (Fun e (uncurry take (readNumber bc)) : s)
evalBC (RETURN : _) _ (v : (RA re rbc) : s) = evalBC rbc re (v : s)
evalBC (SHIFT : bc) e (v : s) = evalBC bc (v : e) s
evalBC (DROP : bc) (v : e) s = evalBC bc e s
evalBC (PRINTN : bc) e st@((I p) : s) = do
  printFD4 $ show p
  evalBC bc e st
evalBC (PRINT : bc) e s = do
  printFD4 $ bc2string (takeWhile (/= NULL) bc)
  evalBC (tail (dropWhile (/= NULL) bc)) e s
evalBC (FIX : bc) e ((Fun fe fb) : s) = evalBC bc e (Fun (eFix fb fe) fb : s)
evalBC (IFZ : bc) e ((I v) : s)
  | v == 0 = evalBC bc e s
  | otherwise = evalBC (uncurry drop (readNumber bc)) e s
evalBC (JUMP : bc) e s = evalBC (uncurry drop (readNumber bc)) e s
evalBC _ _ _ = error "El programa es invalido, papu"

runBC :: (MonadFD4 m) => Bytecode -> m ()
runBC bc = do
  t <- evalBC bc [] []
  return ()
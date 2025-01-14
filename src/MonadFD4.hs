{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

-- |
-- Module      : MonadFD4
-- Description : Mónada con soporte para estado, errores, e IO.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
--
-- Definimos la clase de mónadas 'MonadFD4' que abstrae las mónadas con soporte para estado, errores e IO,
-- y la mónada 'FD4' que provee una instancia de esta clase.
module MonadFD4
  ( FD4,
    FD4Profiled,
    printProfile,
    getProfiling,
    addStep,
    addOpp,
    addMaxStack,
    addClos,
    printStr,
    runFD4,
    runFD4Profiling,
    lookupDecl,
    lookupTy,
    printFD4,
    setLastFile,
    getLastFile,
    addTy,
    getTyEnv,
    setInter,
    getInter,
    getMode,
    getOpt,
    eraseLastFileDecls,
    lookupTypes,
    failPosFD4,
    failFD4,
    addDecl,
    getCek,
    catchErrors,
    MonadFD4,
    module Control.Monad.Except,
    module Control.Monad.State,
  )
where

import Common
import qualified Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Errors (Error (..))
import Global
import Lang
import System.IO

-- * La clase 'MonadFD4'

-- | La clase de mónadas 'MonadFD4' clasifica a las mónadas con soporte para una configuración Global 'Global.Conf',
--    para operaciones @IO@, estado de tipo 'Global.GlEnv', y errores de tipo 'Errors.Error'.
--
-- Las mónadas @m@ de esta clase cuentan con las operaciones:
--   - @ask :: m Conf@
--   - @get :: m GlEnv@
--   - @put :: GlEnv -> m ()@
--   - @throwError :: Error -> m a@
--   - @catchError :: m a -> (Error -> m a) -> m a@
--   - @liftIO :: IO a -> m a@
--
-- y otras operaciones derivadas de ellas, como por ejemplo
--   - @modify :: (GlEnv -> GlEnv) -> m ()@
--   - @gets :: (GlEnv -> a) -> m a@
class (MonadIO m, MonadState GlEnv m, MonadError Error m, MonadReader Conf m) => MonadFD4 m where
  addStep :: m ()

  addOpp :: m ()

  addMaxStack :: Int -> m ()

  addClos :: m ()

getOpt :: (MonadFD4 m) => m Bool
getOpt = asks opt

getMode :: (MonadFD4 m) => m Mode
getMode = asks modo

printProfile :: (MonadFD4 m) => m ()
printProfile = do
  p <- getProfiling
  Control.Monad.when p $ do
    (Prof s o m c) <- getProf
    printFD4 "--------------------------"
    printFD4 $ "Numero de pasos: " ++ show s
    printFD4 $ "Numero de operaciones: " ++ show o
    printFD4 $ "Tamaño maximo de stack: " ++ show m
    printFD4 $ "Numero de clausuras: " ++ show c
    printFD4 "--------------------------"
    return ()

getProfiling :: (MonadFD4 m) => m Bool
getProfiling = asks profiling

getProf :: (MonadFD4 m) => m Profile
getProf = gets profile

setProf :: (MonadFD4 m) => Profile -> m ()
setProf p = modify (\s -> s {profile = p})

setInter :: (MonadFD4 m) => Bool -> m ()
setInter b = modify (\s -> s {inter = b})

getInter :: (MonadFD4 m) => m Bool
getInter = gets inter

printStr :: (MonadFD4 m) => String -> m ()
printStr = liftIO . putStr

printFD4 :: (MonadFD4 m) => String -> m ()
printFD4 = liftIO . putStrLn

setLastFile :: (MonadFD4 m) => FilePath -> m ()
setLastFile filename = modify (\s -> s {lfile = filename, cantDecl = 0})

getLastFile :: (MonadFD4 m) => m FilePath
getLastFile = gets lfile

addTy :: (MonadFD4 m) => Name -> Ty -> m ()
addTy n t = modify (\s -> s {types = lookAndReplace (n, t) (types s)})

getTyEnv :: (MonadFD4 m) => m TyEnv
getTyEnv = gets types

lookAndReplace :: (Eq a) => (a, b) -> [(a, b)] -> [(a, b)]
lookAndReplace t [] = [t]
lookAndReplace t@(ea, eb) (x@(a, b) : xs)
  | ea == a = t : xs
  | otherwise = x : lookAndReplace t xs

addDecl :: (MonadFD4 m) => Decl TTerm -> m ()
addDecl d@(Decl {}) = modify (\s -> s {glb = d : glb s, cantDecl = cantDecl s + 1})
addDecl d@(TyDecl {}) = return ()

eraseLastFileDecls :: (MonadFD4 m) => m ()
eraseLastFileDecls = do
  s <- get
  let n = cantDecl s
      (_, rem) = splitAt n (glb s)
  modify (\s -> s {glb = rem, cantDecl = 0})

lookupDecl :: (MonadFD4 m) => Name -> m (Maybe TTerm)
lookupDecl nm = do
  s <- get
  case filter (hasName nm) (glb s) of
    (Decl {declBody = e}) : _ -> return (Just e)
    [] -> return Nothing
  where
    hasName :: Name -> Decl a -> Bool
    hasName nm (Decl {declName = nm'}) = nm == nm'

lookupTy :: (MonadFD4 m) => Name -> m (Maybe Ty)
lookupTy nm = do
  s <- get
  return $ lookup nm (tyEnv s)

lookupTypes :: (MonadFD4 m) => Name -> m (Maybe Ty)
lookupTypes nm = do
  tys <- gets types
  return $ lookup nm tys

failPosFD4 :: (MonadFD4 m) => Pos -> String -> m a
failPosFD4 p s = throwError (ErrPos p s)

failFD4 :: (MonadFD4 m) => String -> m a
failFD4 = failPosFD4 NoPos

catchErrors :: (MonadFD4 m) => m a -> m (Maybe a)
catchErrors c =
  catchError
    (Just <$> c)
    ( \e ->
        liftIO $
          hPrint stderr e
            >> return Nothing
    )

----
-- Importante, no eta-expandir porque GHC no hace una
-- eta-contracción de sinónimos de tipos
-- y Main no va a compilar al escribir `InputT FD4 ()`

-- | El tipo @FD4@ es un sinónimo de tipo para una mónada construida usando dos transformadores de mónada sobre la mónada @IO@.
-- El transformador de mónad @ExcepT Error@ agrega a la mónada IO la posibilidad de manejar errores de tipo 'Errors.Error'.
-- El transformador de mónadas @StateT GlEnv@ agrega la mónada @ExcepT Error IO@ la posibilidad de manejar un estado de tipo 'Global.GlEnv'.
type FD4 = ReaderT Conf (StateT GlEnv (ExceptT Error IO))

type FD4Profiled = StateT GlEnv (ReaderT Conf (ExceptT Error IO))

-- | Esta es una instancia vacía, ya que 'MonadFD4' no tiene funciones miembro.
runFD4' :: FD4 a -> Conf -> IO (Either Error (a, GlEnv))
runFD4' c conf = runExceptT $ runStateT (runReaderT c conf) initialEnv

runFD4 :: FD4 a -> Conf -> IO (Either Error a)
runFD4 c conf = fmap fst <$> runFD4' c conf

runFD4Profiling' :: FD4Profiled a -> Conf -> IO (Either Error (a, GlEnv))
runFD4Profiling' c conf = runExceptT $ runReaderT ( runStateT c initialEnv) conf

runFD4Profiling :: FD4Profiled a -> Conf -> IO (Either Error a)
runFD4Profiling c conf = fmap fst <$> runFD4Profiling' c conf

getCek :: (MonadFD4 m) => m Bool
getCek = asks cek

instance MonadFD4 FD4 where
  -- 'runFD4\'' corre una computación de la mónad 'FD4' en el estado inicial 'Global.initialEnv'

  addStep = return ()

  addOpp = return ()

  addMaxStack size = return ()

  addClos = return ()

instance MonadFD4 FD4Profiled where
  addStep = do
    p@(Prof s _ _ _) <- getProf
    _ <- setProf $ p {steps = s + 1}
    return ()

  addOpp = do
    p@(Prof _ o _ _) <- getProf
    _ <- setProf $ p {operations = o + 1}
    return ()

  addMaxStack size = do
    p@(Prof _ _ s _) <- getProf
    _ <- setProf $ p {maxStackSize = max s size}
    return ()

  addClos = do
    p@(Prof _ _ _ c) <- getProf
    _ <- setProf $ p {closures = c + 1}
    return ()
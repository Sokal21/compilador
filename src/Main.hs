{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Main
-- Description : Compilador de FD4.
-- Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
-- License     : GPL-3
-- Maintainer  : mauro@fceia.unr.edu.ar
-- Stability   : experimental
module Main where

-- import Control.Monad

-- import Bytecompile (bcRead, bcWrite, bytecompileModule, runBC, fileExtesion)
import Bytecompile8b (bcRead, bcWrite, bytecompileModule, runBC, fileExtesion)
import CEK (seek, value2term)
import Control.Exception (IOException, catch)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans
import Data.Char (isSpace)
import Data.List (intercalate, isPrefixOf, nub)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Elab (elab, elabDecl)
import Errors
import Eval (eval)
import Global
import Lang
import MonadFD4
import Options.Applicative
import PPrint (freshSTy, pp, ppDecl, ppTy)
import Parse (P, declOrTm, program, runP, tm)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.FilePath (dropExtension)
import System.IO (hPrint, hPutStrLn, stderr)
import TypeChecker (tc, tcDecl)

prompt :: String
prompt = "FD4> "

type EvalFn m = TTerm -> m TTerm

-- | Parser de banderas
parseMode :: Parser (Mode, Bool)
parseMode =
  (,)
    <$> ( flag' Typecheck (long "typecheck" <> short 't' <> help "Chequear tipos e imprimir el término")
            <|> flag' Interactive (long "interactive" <> short 'i' <> help "Ejecutar en forma interactiva")
            <|> flag InteractiveCEK InteractiveCEK (long "interactiveCEK" <> short 'k' <> help "Ejecutar interactivamente en la CEK")
            <|> flag Bytecompile Bytecompile (long "bytecompile" <> short 'm' <> help "Compilar a la BVM")
            <|> flag RunVM RunVM (long "runVM" <> short 'r' <> help "Ejecutar bytecode en la BVM")
            <|> flag Eval Eval (long "eval" <> short 'e' <> help "Evaluar programa")
            <|> flag CEKEval CEKEval (long "cek" <> short 'l' <> help "Evaluar programa con CEK")
            -- <|> flag' CC ( long "cc" <> short 'c' <> help "Compilar a código C")
            -- <|> flag' Canon ( long "canon" <> short 'n' <> help "Imprimir canonicalización")
            -- <|> flag' Assembler ( long "assembler" <> short 'a' <> help "Imprimir Assembler resultante")
            -- <|> flag' Build ( long "build" <> short 'b' <> help "Compilar")
        )
    <*> pure False

-- reemplazar por la siguiente línea para habilitar opción
-- <*> flag False True (long "optimize" <> short 'o' <> help "Optimizar código")

-- | Parser de opciones general, consiste de un modo y una lista de archivos a procesar
parseArgs :: Parser (Mode, Bool, [FilePath])
parseArgs = (\(a, b) c -> (a, b, c)) <$> parseMode <*> many (argument str (metavar "FILES..."))

main :: IO ()
main = execParser opts >>= go
  where
    opts =
      info
        (parseArgs <**> helper)
        ( fullDesc
            <> progDesc "Compilador de FD4"
            <> header "Compilador de FD4 de la materia Compiladores 2022"
        )

    go :: (Mode, Bool, [FilePath]) -> IO ()
    go (Bytecompile, opt, files) =
      runOrFail (Conf opt Bytecompile) $ mapM_ compileBytecode files
    go (RunVM, opt, files) =
      runOrFail (Conf opt RunVM) $ mapM_ runBVM files
    go (Interactive, opt, files) =
      runOrFail (Conf opt Interactive) (runInputT defaultSettings (repl eval files))
    go (InteractiveCEK, opt, files) =
      runOrFail (Conf opt InteractiveCEK) (runInputT defaultSettings (repl evalCEK files))
    go (m, opt, files) =
      runOrFail (Conf opt m) $ mapM_ compileFile files

runBVM :: (MonadFD4 m) => FilePath -> m ()
runBVM f = do
  bc <- liftIO $ bcRead f
  runBC bc

runOrFail :: Conf -> FD4 a -> IO a
runOrFail c m = do
  r <- runFD4 m c
  case r of
    Left err -> do
      liftIO $ hPrint stderr err
      exitWith (ExitFailure 1)
    Right v -> return v

repl :: (MonadFD4 m, MonadMask m) => EvalFn m -> [FilePath] -> InputT m ()
repl f args = do
  lift $ setInter True
  lift $ catchErrors $ mapM_ compileFile args
  s <- lift get
  when (inter s) $
    liftIO $
      putStrLn
        ( "Entorno interactivo para FD4.\n"
            ++ "Escriba :? para recibir ayuda."
        )
  loop
  where
    loop = do
      minput <- getInputLine prompt
      case minput of
        Nothing -> return ()
        Just "" -> loop
        Just x -> do
          c <- liftIO $ interpretCommand x
          b <- lift $ catchErrors $ handleCommand f c
          maybe loop (`when` loop) b

loadFile :: (MonadFD4 m) => FilePath -> m [SDecl STerm]
loadFile f = do
  let filename = reverse (dropWhile isSpace (reverse f))
  x <-
    liftIO $
      catch
        (readFile filename)
        ( \e -> do
            let err = show (e :: IOException)
            hPutStrLn stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err)
            return ""
        )
  setLastFile filename
  parseIO filename program x

typecheckDecl :: (MonadFD4 m) => SDecl STerm -> m (Decl TTerm)
typecheckDecl t = do
  e <- elabDecl t
  tcDecl e

compileBytecode :: (MonadFD4 m) => FilePath -> m ()
compileBytecode f = do
  setInter False
  printFD4 ("Abriendo " ++ f ++ "...")
  decls <- loadFile f
  tcdecl <- mapM tcAndAdd decls
  bc <- bytecompileModule tcdecl
  liftIO $ bcWrite bc (dropExtension f ++ fileExtesion)
  where
    tcAndAdd d = do
      tcd <- typecheckDecl d
      addDecl tcd
      return tcd

compileFile :: (MonadFD4 m) => FilePath -> m ()
compileFile f = do
  i <- getInter
  setInter False
  when i $ printFD4 ("Abriendo " ++ f ++ "...")
  decls <- loadFile f
  mapM_ handleDecl decls
  setInter i

parseIO :: (MonadFD4 m) => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
  Left e -> throwError (ParseErr e)
  Right r -> return r

evalDecl :: (MonadFD4 m) => EvalFn m -> Decl TTerm -> m (Decl TTerm)
evalDecl f t@(TyDecl {}) = do
  return t
evalDecl f (Decl p x e) = do
  e' <- f e
  return (Decl p x e')

handleDecl :: (MonadFD4 m) => SDecl STerm -> m ()
handleDecl d = do
  m <- getMode
  case m of
    Interactive -> do
      -- (Decl p x tt) <- typecheckDecl d
      dd <- typecheckDecl d
      case dd of
        (Decl p x tt) -> do
          te <- eval tt
          addDecl (Decl p x te)
        (TyDecl p x tt) -> do
          addTy x tt
    Typecheck -> do
      f <- getLastFile
      printFD4 ("Chequeando tipos de " ++ f)
      td <- typecheckDecl d
      addDecl td
      -- opt <- getOpt
      -- td' <- if opt then optimize td else td
      ppterm <- ppDecl td -- td'
      printFD4 ppterm
    CEKEval -> do
      td <- typecheckDecl d
      -- td' <- if opt then optimizeDecl td else return td
      ed <- evalDecl evalCEK td
      case ed of
        (Decl p x tt) -> do
          addDecl ed
        (TyDecl p x tt) -> do
          addTy x tt
      addDecl ed
    Eval -> do
      td <- typecheckDecl d
      -- td' <- if opt then optimizeDecl td else return td
      ed <- evalDecl eval td
      case ed of
        (Decl p x tt) -> do
          addDecl ed
        (TyDecl p x tt) -> do
          addTy x tt
    InteractiveCEK -> do
      dd <- typecheckDecl d
      case dd of
        (Decl p x tt) -> do
          te <- seek tt [] []
          addDecl (Decl p x (value2term te))
        (TyDecl p x tt) -> do
          addTy x tt

data Command
  = Compile CompileForm
  | PPrint String
  | Type String
  | Reload
  | Browse
  | Quit
  | Help
  | Noop

data CompileForm
  = CompileInteractive String
  | CompileFile String

data InteractiveCommand = Cmd [String] String (String -> Command) String

-- | Parser simple de comando interactivos
interpretCommand :: String -> IO Command
interpretCommand x =
  if ":" `isPrefixOf` x
    then do
      let (cmd, t') = break isSpace x
          t = dropWhile isSpace t'
      --  find matching commands
      let matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
      case matching of
        [] -> do
          putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda.")
          return Noop
        [Cmd _ _ f _] ->
          do return (f t)
        _ -> do
          putStrLn
            ( "Comando ambigüo, podría ser "
                ++ intercalate ", " ([head cs | Cmd cs _ _ _ <- matching])
                ++ "."
            )
          return Noop
    else return (Compile (CompileInteractive x))

commands :: [InteractiveCommand]
commands =
  [ Cmd [":browse"] "" (const Browse) "Ver los nombres en scope",
    Cmd
      [":load"]
      "<file>"
      (Compile . CompileFile)
      "Cargar un programa desde un archivo",
    Cmd [":print"] "<exp>" PPrint "Imprime un término y sus ASTs sin evaluarlo",
    Cmd [":reload"] "" (const Reload) "Vuelve a cargar el último archivo cargado",
    Cmd [":type"] "<exp>" Type "Chequea el tipo de una expresión",
    Cmd [":quit", ":Q"] "" (const Quit) "Salir del intérprete",
    Cmd [":help", ":?"] "" (const Help) "Mostrar esta lista de comandos"
  ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs =
  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n"
    ++ "c es el primer caracter del nombre completo.\n\n"
    ++ "<expr>                  evaluar la expresión\n"
    ++ "let <var> = <expr>      definir una variable\n"
    ++ unlines
      ( map
          ( \(Cmd c a _ d) ->
              let ct = intercalate ", " (map (++ if null a then "" else " " ++ a) c)
               in ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d
          )
          cs
      )

-- | 'handleCommand' interpreta un comando y devuelve un booleano
-- indicando si se debe salir del programa o no.
handleCommand :: (MonadFD4 m) => EvalFn m -> Command -> m Bool
handleCommand ef cmd = do
  s@GlEnv {..} <- get
  case cmd of
    Quit -> return False
    Noop -> return True
    Help -> printFD4 (helpTxt commands) >> return True
    Browse -> do
      printFD4 (unlines (reverse (nub (map declName glb))))
      return True
    Compile c ->
      do
        case c of
          CompileInteractive e -> compilePhrase ef e
          CompileFile f -> compileFile f
        return True
    Reload -> eraseLastFileDecls >> (getLastFile >>= compileFile) >> return True
    PPrint e -> printPhrase e >> return True
    Type e -> typeCheckPhrase e >> return True

compilePhrase :: (MonadFD4 m) => EvalFn m -> String -> m ()
compilePhrase f x = do
  dot <- parseIO "<interactive>" declOrTm x
  case dot of
    Left d -> handleDecl d
    Right t -> handleTerm f t

evalCEK :: (MonadFD4 m) => TTerm -> m TTerm
evalCEK t = do
  te <- seek t [] []
  _ <- trace (show te) return ()
  return $ value2term te

handleTerm :: (MonadFD4 m) => EvalFn m -> STerm -> m ()
handleTerm f t = do
  t' <- elab t
  s <- get
  tt <- tc t' (tyEnv s) (types s)
  te <- f tt
  ppte <- pp te
  printFD4 (ppte ++ " : " ++ ppTy (freshSTy (getTy tt)))

printPhrase :: (MonadFD4 m) => String -> m ()
printPhrase x =
  do
    x' <- parseIO "<interactive>" tm x
    ex <- elab x'
    s <- get
    tex <- tc ex (tyEnv s) (types s)
    t <- case x' of
      (SV p f) -> fromMaybe tex <$> lookupDecl f
      _ -> return tex
    printFD4 "STerm:"
    printFD4 (show x')
    printFD4 "TTerm:"
    printFD4 (show t)

typeCheckPhrase :: (MonadFD4 m) => String -> m ()
typeCheckPhrase x = do
  t <- parseIO "<interactive>" tm x
  t' <- elab t
  s <- get
  tt <- tc t' (tyEnv s) (types s)
  let ty = getTy tt
  printFD4 (ppTy (freshSTy ty))

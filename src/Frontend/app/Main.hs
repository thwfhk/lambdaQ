module Main where

import Lexer
import Parser
import Syntax
import Context
import Desugar
import TypeChecker
import CodeGenerator
import QASMSyntax
import PrettyPrinter
import Text.Parsec
import Control.Monad.Except
import Control.Monad.State
import Data.Functor.Identity ( Identity(runIdentity) )
import System.Environment
import Debug.Trace


runMyParser :: String -> Contexts -> String -> Either ParseError [Command]
runMyParser = flip (runParser parseCommands)

runMyParserREPL :: String -> Either ParseError Term
runMyParserREPL = runParser parseTerm emptyctxs "CandyQwQ"

runQwQ :: c -> ExceptT e (StateT c Identity) a -> Either e a
runQwQ initial = fst .runIdentity . flip runStateT initial . runExceptT

repl :: IO ()
repl = do
  prog <- getLine
  case runMyParserREPL prog of
    Left err -> print err
    Right term -> do
      putStrLn $ "[PARSE SUCCESS 🥳]:\n  " ++ show term
      let mty = runQwQ emptyctxs $ typeOf term
      case mty of
        Right ty -> putStrLn $ "[TYPE SUCCESS 🥳]:\n  " ++ show ty
        Left e -> putStrLn $ "[TYPE FAILED 😵]: " ++ e
      let mqasm = runQwQ emptystate $ term2QASM term
      case mqasm of
        Right (qasm, _) -> putStrLn $ "[GENARATION SUCCESS 🥳]:\n  " ++ show qasm ++ "\n" ++ printQASM "" qasm
        Left e -> putStrLn $ "[GENERATION FAILED 😵]: " ++ e
  putStrLn ""
  repl

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourceFileName] -> do
      sourceFile <- readFile sourceFileName
      -- print $ "Source: " ++ sourceFile
      case (runMyParser sourceFileName emptyctxs sourceFile) of
        Left err -> putStrLn $ "[PARSE FAILED 😵]: " ++ show err
        Right cmds -> do
          putStrLn $ "[PARSE SUCCESS 🥳]: " ++ show (length cmds)
            ++ " function" ++ if length cmds == 1 then "" else "s" ++ " founded."
          let cmds' = deSugar cmds
          case (typeInference cmds') of
            Left err -> putStrLn $ "[TYPE FAILED 😵]: " ++ err
            Right tycms -> do
              putStrLn $ "[TYPE SUCCESS 🥳]:"
              let tys = map fst tycms
              let cmds'' = map snd tycms
              mapM_ (\ ((Def s _), ty) -> putStrLn $ "  " ++ s ++ " : " ++ printType ty) (zip cmds'' tys)
              case (codeGeneration cmds'') of
                Left err -> putStrLn $ "[GENERATION FAILED 😵]: " ++ err
                Right qasm -> putStrLn $ "[GENERATION SUCCESS 🥳]:\n" ++ printQASM "  " qasm
    _ -> putStrLn "source-file name not founded, enter REPL" >> repl

deSugar :: [Command] -> [Command]
deSugar = map (\ (Def s t) -> Def s (desugar t))

typeInference :: [Command] -> Either Err [(Type, Command)]
typeInference = fst . foldl tyinf (Right [], emptyctxs) 
  where
    tyinf :: (Either Err [(Type, Command)], Contexts) -> Command
          -> (Either Err [(Type, Command)], Contexts)
    tyinf (mtys, ctxs@(gamma, omega)) cmd = case mtys of
      Left err -> (Left err, ctxs)
      Right tys -> case cmd of
        Def x t -> let mtytm = runQwQ ctxs (typeOf t)
                   in case mtytm of
                    Left err -> (Left ("Definition " ++ x ++ " error: " ++ err), ctxs)
                    Right (ty, nt) -> ( Right (tys ++ [(ty, Def x nt)])
                                      , (addBinding gamma (x, VarBind ty), omega))

codeGeneration :: [Command] -> Either Err Program
codeGeneration cmds =
  let declarations = map (\ (Def name t) -> (name, Right t)) (reverse $ init cmds)
  in let Def _ t = last cmds
  in fst <$> (runQwQ (emptyregs, declarations) $ term2QASM t)

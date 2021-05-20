module Main where

import Lexer
import Parser
import Syntax
import Context
import TypeChecker
import CodeGenerator
import QASMPrinter
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
      putStrLn $ "[Parse SUCCESS 🥳]:\n  " ++ show term
      let mty = runQwQ emptyctxs $ typeOf term
      case mty of
        Right ty -> putStrLn $ "[Type SUCCESS 🥳]:\n  " ++ show ty
        Left e -> putStrLn $ "[Type FAIL 😵]: " ++ e
      let mqasm = runQwQ emptystate $ term2QASM term
      case mqasm of
        Right qasm -> putStrLn $ "[Generation SUCCESS 🥳]:\n  " ++ show qasm ++ "\n" ++ printQASM qasm
        Left e -> putStrLn $ "[Generation FAIL 😵]: " ++ e
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
        Left err -> print err
        Right cmds -> do
          case (typeInference cmds) of
            Left err -> print err
            Right tys -> print tys
          -- return ()
    _ -> putStrLn "source-file name not founded, enter REPL" >> repl

typeInference :: [Command] -> Either Err [Type]
typeInference = fst . foldl tyinf (Right [], emptyctxs) 
  where
    tyinf :: (Either Err [Type], Contexts) -> Command -> (Either Err [Type], Contexts)
    tyinf (mtys, ctxs@(gamma, omega)) cmd = case mtys of
      Left err -> (Left err, ctxs)
      Right tys -> case cmd of
        Def x t -> let mty = runQwQ ctxs (typeOf t)
                   in case mty of
                    Left err -> (Left ("Definition " ++ x ++ " error: " ++ err), ctxs)
                    Right ty -> (Right (tys ++ [ty]), (addBinding gamma (x, VarBind ty), omega))

-- codeGeneration :: [Command] -> Either Err Program
-- codeGeneration cmds = let main = findmain cmds in 
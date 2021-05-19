module Main where

import Lexer
import Parser
import Context
import TypeChecker
import CodeGenerator
import QASMPrinter
import Control.Monad.Except
import Control.Monad.State
import Data.Functor.Identity ( Identity(runIdentity) )

runQwQ :: c -> ExceptT e (StateT c Identity) a -> Either e a
runQwQ initial = fst .runIdentity . flip runStateT initial . runExceptT

main :: IO ()
main = do
  prog <- getLine
  case runMyParser prog of
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
  main
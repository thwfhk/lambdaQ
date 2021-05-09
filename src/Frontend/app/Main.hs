module Main where

import Lexer
import Parser
import Context
import TypeChecker
import Control.Monad.Except
import Control.Monad.State
import Data.Functor.Identity

main :: IO ()
main = do
  prog <- getLine
  -- putStrLn prog
  case runMyParser prog of
    Left err -> print err
    Right term -> do
      putStrLn $ "[SUCCESS] " ++ show term
      let mty = (fst .runIdentity . flip runStateT emptyctxs . runExceptT) $ typeOf term
      case mty of
        Right ty -> putStrLn $ "[TYPE SUCCESS🥳] " ++ show ty
        Left e -> putStrLn $ "[TYPE FAIL😵] " ++ e
  putStrLn ""
  main
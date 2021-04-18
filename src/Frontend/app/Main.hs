module Main where

import Lexer
import Parser

main :: IO ()
main = do
  prog <- getLine
  putStrLn prog
  case runMyParser prog of
    Left err -> print err
    Right term -> print term

module Main where

import System.Environment (getArgs)
import Parser (parseComm)
import Eval2
import AST

---------------------------------------------------------
-- MAIN
---------------------------------------------------------

main :: IO ()
main = do
  arg:_ <- getArgs
  run arg

---------------------------------------------------------
-- Ejecutar programa desde archivo
---------------------------------------------------------

run :: FilePath -> IO ()
run file = do
  source <- readFile file
  case parseComm file source of
    Left err -> print err
    Right ast ->
      let (res, finalState, logg) = runProgram ast
      in case res of
           Left err -> putStrLn ("Error: " ++ err)
           Right _  -> do
             putStrLn "Salida:"
             mapM_ printLog logg
             putStrLn "\nEstado final:"
             print finalState

---------------------------------------------------------
-- Mostrar log
---------------------------------------------------------

printLog :: (Int,String) -> IO ()
printLog (k,v) =
  putStrLn ("(step=" ++ show k ++ ", value=\"" ++ v ++ "\")")

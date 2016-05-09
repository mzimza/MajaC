--Author Maja Zalewska
--Index 336088
--Interpreter for MajaC language

{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.IO (stdin, hGetContents)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)

import LexMajaC
import ParMajaC
import SkelMajaC
import PrintMajaC
import AbsMajaC
import EvalMajaC
import MajaCTypeChecker
import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

runFile :: (Print Program, Show Program) => ParseFun Program -> FilePath -> IO ()
runFile p f = putStrLn f >> readFile f >>= run p

run :: (Print Program, Show Program) => ParseFun Program -> String -> IO ()
run p s = let ts = myLLexer s in case p ts of
                  Bad s -> do putStrLn "\nParse Failed...\n"
                              putStrLn "Tokens:"
                              putStrLn $ show ts
                              putStrLn s
                              exitFailure
                  Ok tree -> do putStrLn "\nParse Successful!"
                                showTree tree
                                checkProg tree
                                execProg tree
                                exitSuccess

showTree :: (Show Program, Print Program) => Program -> IO ()
showTree tree = do putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
                   putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree

main :: IO ()
main = do
         args <- getArgs
         case args of
            [] -> hGetContents stdin >>= run pProgram
            fs -> mapM_ (runFile pProgram) fs

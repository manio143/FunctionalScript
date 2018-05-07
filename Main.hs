module Main where

import AST
import Parser
import Translator
import ProgramState

import System.IO
import System.Environment
import System.Directory

main = do
    args <- getArgs
    case args of
        ["-h"] -> usage
        (file:args) -> interpret file args
        _ -> usage
        
usage = do
    putStrLn "Interpreter języka FunScript"
    putStrLn "Autor: Marian Dziubiak"
    putStrLn ""
    putStrLn "Usage:"
    putStrLn "  interpreter program [arg1 [arg2 [...]]]"

interpret file args = do 
    exists <- doesFileExist file
    if not exists then do
        putStrLn "ERR: No such file"
        usage
    else
        -- readAllLines
        -- parse
        -- validate
        -- execute
        do
            h <- openFile file ReadMode
            contents <- hGetContents h
            let ast = parseFile file contents
            case ast of
                Right program -> do
                    store <- translate program
                    runProgram store args
                Left err -> print err
module Main where

import Prelude hiding (getContents)

import AST
import Parser
import Translator
import ProgramState

import System.IO hiding (getContents)
import System.Environment
import System.Directory
import System.Exit

import Data.Either (isLeft)

main = do
    args <- getArgs
    case args of
        ["-h"] -> usage
        ("-w":files:file:args) -> interpretWith (wordsWhen (== ',') files) file args
        (file:args) -> interpret file args
        _ -> usage
        
usage = do
    putStrLn "Interpreter języka FunScript"
    putStrLn "Autor: Marian Dziubiak"
    putStrLn ""
    putStrLn "Usage:"
    putStrLn "  interpreter [-w lib1[,lib2[,...]]] program [arg1 [arg2 [...]]]"

interpret file args = do 
    exists <- assertExistance [file]
    if not exists then usage
    else do
        contents <- getContents file
        let ast = parseFile file contents
        case ast of
            Right program -> do
                store <- translate program
                runProgram store args
            Left err -> print err >> failure

interpretWith libs file args = do
    exist <- assertExistance (file:libs)
    if not exist then usage >> failure
    else do
        asts <- mapM (\f -> getContents f >>= return . parseFile f) (file:libs)
        let errs = filter isLeft asts
        if length errs > 0 then mapM (\(Left e) -> print e) errs >> failure
        else do
            let program = Program $ concat $ map (\(Right (Program ds))->ds) asts
            store <- translate program
            runProgram store args

getContents file = do
    h <- openFile file ReadMode
    hGetContents h

assertExistance (f:fs) = do
    exists <- doesFileExist f
    if not exists then do
        putStrLn $ "ERROR: No such file: " ++ f ++ "\n"
        return False
    else assertExistance fs
assertExistance [] = return True

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                        "" -> []
                        s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

failure = exitWith (ExitFailure 1)
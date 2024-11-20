module Main where

import Java
import Tests
import Defun
import ANF
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
     template <- readFile "src/template.java"
     args <- getArgs
     let progName = read (head args) :: Int
     putStr (toJava template $ testProg progName)

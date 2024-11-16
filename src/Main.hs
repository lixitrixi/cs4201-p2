module Main where

import Java
import Tests
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
     template <- readFile "src/template.java"
     args <- getArgs
     let progName = head args
     putStr (toJava template $ testProg progName)

module Main where

import BoggleSolver.WordFinder
import IO
import Control.Exception hiding (catch)
import Control.Concurrent
import Network
import System.Posix
import System( getArgs )


main =  do t <- trie
           args <- System.getArgs
           let words = doItAll (args !! 0) t 
           putStrLn "--------"
           mapM_ putStrLn words
           putStrLn "--------"
           return ()
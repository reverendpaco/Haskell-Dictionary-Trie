module Main where

import BoggleSolver.WordFinder
import IO
import Control.Exception hiding (catch)
import Control.Concurrent
import Network
import System.Posix


main = withSocketsDo (installHandler sigPIPE Ignore Nothing >> main')
main' = listenOn (PortNumber 9900) >>= acceptConnections

acceptConnections sock = do
        putStrLn "trying to accept" -- debug msg
        conn@(h,host,port) <- accept sock
        print conn -- debug msg
        forkIO $ catch (talk conn `finally` hClose h) (\e -> print e)
        acceptConnections sock

talk conn@(h,_,_) = hGetLine h >>= calculateWords h >> hFlush h >> talk conn

calculateWords h str = do t <- trie
                          let back = hPutStrLn h
                          let words = doItAll str t 
                          back "--------"
                          mapM_ back words
                          back "--------"
                          return ()
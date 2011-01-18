module BoggleSolver.WordFinder ( doItAll, trie ) where
{- 
Boggle Solver:
* Identity
* Relation

How do we know whether or not we've visited a node?
  -- answer: give that node an identity...
  -- how do we give a node an identity?  
        answer: not by its contents... it must be generated separately... 
it must be a name...  we could use coordinates..

* relation
  What about representing the graph, not as a graph, but a 
set of nodes and a set of relations..?

For instance:
   A -- B
    \   |
     \  |
      \ |
        C

Could be three nodes:  A, B, C 
and three relations stored separately

   (A,B)
   (A,C)
   (B,C)
--- another example:
   AB
   CD

would be the relation set:
  {  (A,B), (A,D), (A,C), (B,C), (B,D), (C,D) }
-}

import List (partition,delete)
import qualified Trie.Trie as T
import Trie.TrieUtility
import Data.Char
import System.Environment 
import Control.Parallel.Strategies
import Control.Parallel
import IO
import Control.Exception hiding (catch)
import Control.Concurrent
import Network
import System.Posix



isInRelVar (x,y) m | (m==x) || (m==y) = True 
isInRelVar _ _ = False
           
theOther (x,y) z | z==x = y    
theOther (x,y) z | z==y = x

x `neighbor` y =  ident y `oneAway` ident x
oneAway (x,y) (a,b) | dist > 0 && fdist <=1 && sdist <=1  = True
                    | otherwise = False
                    where dist = fdist + sdist
                          fdist = abs(x-a)
                          sdist = abs(y-b)

convert (x,y,cs) = WNode {ident = (x,y), contents = cs}

type Identity = (Int,Int)
type Content = String
data WNode = WNode { ident::Identity, contents:: Content } 
type Relation  = (WNode, WNode)
type Path = [WNode]
type Board = ( [WNode], [Relation])

instance Eq WNode where
    node1 == node2 = (ident node1) == (ident node2)
instance Show WNode where
    show n = contents n

generate (x:xs)  = rels : generate xs
    where rels = [(x,neighbors) | neighbors <- filter (neighbor x) xs]
generate [] = []

neighbors :: WNode -> [Relation] -> [WNode]
neighbors x rels =  map (`theOther` x) $ filter (`isInRelVar` x) rels

-- the last thing in this type signature is
-- a list of a tuple of a found neigbor node, and the depleted
-- relation (i.e. that relation without the neighbor node)
nsAndNewRels ::  WNode -> [Relation] -> [ ( WNode, [Relation] ) ]
nsAndNewRels x rels = map ( deplete x ) markedRels
    where deplete x rel = (theOther rel x, rels \\ markedRels) 
          markedRels =  filter (`isInRelVar` x) rels


wordFromPath = foldr1 (++) . reverse . map contents
isPrefix trieReal = (T.isPrefix trieReal) . wordFromPath
isWord trieReal = (T.isWord trieReal) . wordFromPath

[] \\ _  = []
xs \\ [] = xs
(x:xs) \\ (y:ys)
  | x == y    = xs \\ ys
  | otherwise = (x : (xs \\ [y])) \\ ys

-- take the relations that still apply, a starting path, 
-- and return an array of paths that are valid words
--generateWords :: [Relation] -> Path -> [Path]
generateWords trie remainingRels path@(node:_) = words
    where words = dropEmpty
          dropEmpty  = filter ((> 0) . length) $
                  if isWordT path then 
                      path : neighborSearchResults
                  else
                      neighborSearchResults
          neighborSearchResults = case isPrefixT path || isWordT path of
                                    False -> [[]]
                                    True -> neighborSearch
          neighborSearch = concat $  newPaths
          newPaths = map callGen foundNeighbors
          foundNeighbors = nsAndNewRels node remainingRels 
          callGen ( node, depleteRels ) = generateWords trie depleteRels (node:path)
          isWordT = isWord trie
          isPrefixT = isPrefix trie
 

basicEnglishWords =["a",
                    "steal",
                    "steales",
                    "any"]

makeTrie a = foldr T.addWordToTrie T.degenerateTree a

pathsOnBoard trie (nodes,rels) = concat $ map ((generateWords trie rels) . (:[])) nodes
wordsOnBoard trie (nodes,rels) =
    map (concat . map contents . reverse)  $ pathsOnBoard trie (nodes,rels)


trie = do 
  trie <- readInTrie "trie.out"
  return trie
main2 trie (nodes,rels) = do 
  t <- trie
  let words = wordsOnBoard t (nodes,rels) 
  return words 

doItAll str trie = wordsOnBoard trie (createBoard str)

createBoard :: String -> Board
createBoard [] = ([],[])
createBoard list = 
    let dim = sqrt $ fromIntegral $ length list
        rounded = round dim
        imperfect = rem (length list)  rounded > 0
        width = rounded
        height = if imperfect then rounded + 1 else rounded 
        aBoard = map (\((x,y),z) -> (x,y,z:[])) $ 
                 zip [(x,y) | x <- [1..height], y <- [1..width]] $ map toLower list
        nodes =  map convert aBoard
        rels = concat $ generate nodes
    in (nodes,rels)

mapP f xs = parBuffer 3 rwhnf $ map f xs

{-

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
-}
module Trie.TrieUtility (writeOutTrie,readInTrie)
 where

import Trie.Trie
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.List as L
import Data.Binary
import Control.Monad
import Codec.Compression.GZip

data Dot = DotPath {fid::String, tid::String} | 
           DotNode{dotId::String,label::String}

instance Show Dot where
    show DotPath{fid=from,tid=to@(x:_)} = "\"" ++ from ++ "\"" 
                                          ++  "->"
                                          ++ "\"" ++ to ++ "\""
                                          ++ "[label=\" " ++ ( x : "\"]")
    show DotNode{dotId=nodeId,label=outputLabel} = "\"" ++ nodeId ++ "\"" 
                                                ++  "[label= \"" ++ outputLabel ++ "\"]" 

generateDotLabel nodeDatum = case (nodeType nodeDatum) of 
                    Top -> "&#8709;"
                    Word ->  wf ++ "(*)" 
                    NonWord ->   wf 
     where wf = (wordFragment nodeDatum)

generateDot :: TrieNode Char -> String -> [Dot]
generateDot nodeDatum path = myDotNode : myPaths ++ myChildrensDots 
    where myDotNode = DotNode{dotId= path,label=(generateDotLabel nodeDatum)}
          myPaths = map generatePathNode $ (Map.keys . children) nodeDatum
          myChildrensDots = foldr (++) []  $ 
                            map generateChildrenDots $ (Map.assocs  . children) nodeDatum
          generatePathNode x = DotPath{fid=path,tid=x:path}
          generateChildrenDots (k,v) = generateDot v (k:path)
          
generateDotPretty tree = L.intercalate "\n" $ map show  $ generateDot tree "_"


writeOutTrie path b = do 
  (encodeFile path . compress . encode)  b

readInTrie path = do
  ahhh <- fmap decompress $ decodeFile path
  return (decode ahhh :: TrieNode Char)



instance (Ord a, Binary a) => Binary (TrieNode a) where
    put TrieNode{wordFragment=w,
                 children=c,
                 nodeType=t} = put (0::Word8) >> put w >> put c >> put t
    get = do tag <- getWord8
             case tag of
               0 -> liftM3 TrieNode get get get

instance Binary NodeType where
    put Top = putWord8 0
    put Word = putWord8 1
    put NonWord = putWord8 2
    get = do tag <- getWord8
             case tag of
                    0 -> return Top
                    1 -> return Word
                    2 -> return NonWord

module TrieUtility 
 where

import Trie
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.List as L

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
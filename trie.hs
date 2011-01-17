module Trie.Trie (NodeType(..),TrieNode(..),
             degenerateTree,addWordToTrie,
             queryNode,isWord, isPrefix, queryNodeWithPath)
  where


import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.List as L

data QueryMatchType = IsWord|IsPrefix|AintNothing deriving (Show)
data NodeType = Top|Word|NonWord 
                deriving (Show,Eq)
data TrieNode a = TrieNode { wordFragment :: [a], children :: (Map.Map a (TrieNode a)), 
                            nodeType:: NodeType }  deriving (Show,Eq)

-- default constructors 
topTree = TrieNode{wordFragment = [""], children = Map.fromList [], nodeType = Top}
defaultNode = TrieNode {children = Map.empty,nodeType = Word, wordFragment=[]}
wordNode frag = defaultNode{nodeType = Word, wordFragment=frag }
nonWordNode frag = defaultNode{nodeType = NonWord, wordFragment=frag }
topNode  = defaultNode{nodeType = Top }
degenerateTree = topNode

data PrefixSuffixTree a = ExactMatchDatum { shared :: a } |
                          SharedPrefixDatum { shared :: a, suffixT :: a, suffixS :: a } |
                          TargetIsSmallerDatum { pre :: a, suffixS :: a } |
                          SourceIsSmallerDatum { pre :: a, suffixT :: a } |
                          NoMatchDatum deriving (Show,Eq)
data MatchType = TargetIsPrefix | SourceIsPrefix | 
                 SharedPrefix | ExactMatch | NotEqual deriving (Show)

-- utility functions
switchToWord node = node{nodeType = Word} 

                          
takeTogetherWhile :: (Eq a) => [a] -> [a] -> PrefixSuffixTree [a]
takeTogetherWhile [] _  = NoMatchDatum
takeTogetherWhile _ []  = NoMatchDatum
takeTogetherWhile source target = case match of 
                                    ExactMatch     -> ExactMatchDatum {shared=source}
                                    TargetIsPrefix -> TargetIsSmallerDatum {pre=target,
                                                                            suffixS= suffixStringOfSource}
                                    SourceIsPrefix -> SourceIsSmallerDatum {pre=source,
                                                                            suffixT= suffixStringOfTarget}
                                    NotEqual       -> NoMatchDatum
                                    SharedPrefix   -> SharedPrefixDatum { shared = sharedPrefix , 
                                                                          suffixS = utilStrip sharedPrefix source ,
                                                                          suffixT = utilStrip sharedPrefix target}
                  where match = findMatchOnString source target
                        suffixStringOfSource =  utilStrip target source
                        suffixStringOfTarget =  utilStrip source target
                        utilStrip s t =   case L.stripPrefix s t of 
                                                  Just s -> s
                                                  Nothing -> []
                        sharedPrefix  = [ fst x | x <- takeWhile ( \ (x,y) -> x==y) $ zip source target ]
source =><= target = takeTogetherWhile source target

findMatchOnString sourceS@(s:ss) targetS@(t:ts) | sourceS == targetS = ExactMatch
                                                | sourceS `L.isPrefixOf` targetS = SourceIsPrefix
                                                | targetS `L.isPrefixOf` sourceS = TargetIsPrefix
                                                | s /= t = NotEqual
                                                | otherwise = SharedPrefix
insertNewChildWordNode word@(x:xs) childNodes =  (wordNode word) *-> childNodes 

insertNewChildNode  node@TrieNode{wordFragment=word@(x:xs)} childNodes 
    = Map.insert x node childNodes
x *-> y = insertNewChildNode x y
-- match for the top node
addWordToTrie word@(x:xs) node@(TrieNode{children=childNodes,nodeType=Top} )
              | Map.notMember x childNodes = node{children= (insertNewChildWordNode word childNodes)}
              | otherwise =  node{ children=  newlyChangedNode *-> childNodes}
    where subNode s = childNodes ! s
          newlyChangedNode = addWordToTrie word (subNode x)
-- match for the others
addWordToTrie source@(x:xs) node@(TrieNode{children=childNodes, wordFragment=target} )
              = case matchData of
                  TargetIsSmallerDatum {pre=target,
                                        suffixS= suffixStringOfSource@(s:ss)}  
                      | Map.notMember s childNodes -> insertNode node matchData
                      | otherwise -> node{ children =  newlyChangedNode *-> childNodes}
                      where subNode  = (childNodes ! )
                            newlyChangedNode = addWordToTrie suffixStringOfSource (subNode s)
                  otherwise -> insertNode node matchData
                where matchData =  source =><= target


insertNode contextNode ExactMatchDatum{} = switchToWord contextNode
-- the following is guaranteed not to have something at the child node as we
-- checked it above
insertNode contextNode@TrieNode{children=childNodes} 
           TargetIsSmallerDatum {pre=target,
                                 suffixS=suffixStringOfSource} 
    = contextNode{ children=(insertNewChildWordNode suffixStringOfSource childNodes) }
-- the following is for trie nodes where the source is smaller
insertNode contextNode@TrieNode{children=childNodes,nodeType=oldNodeType} 
           SourceIsSmallerDatum {pre=source,
                                 suffixT= suffixStringOfTarget}
    = contextNode{ wordFragment = source,
                   nodeType = Word,
                   children= slidNode *-> childNodes   }
---- this is not RIGHT !!!
      where slidNode = contextNode{wordFragment=suffixStringOfTarget,nodeType=oldNodeType}
-- this will be the complicated case
insertNode contextNode@TrieNode{children=childNodes,nodeType=oldNodeType} 
           SharedPrefixDatum { shared = sharedPrefix , 
                               suffixS = sourceSuffix@(s:ss),
                               suffixT = targetSuffix@(t:ts)}
               = newForkNode {children = ourTwoNewNodes }
    where newForkNode = nonWordNode sharedPrefix
          ourTwoNewNodes = Map.fromList [(s,newSourceSuffixNode),(t,newTargetSuffixNode) ]
          newSourceSuffixNode = wordNode sourceSuffix
          newTargetSuffixNode = contextNode{ wordFragment = targetSuffix}

isWord node a = case queryNode a node of
             IsWord -> True
             otherwise -> False
isPrefix node a = case queryNode a node of
               IsPrefix -> True
               otherwise -> False
queryNode source node = bool
    where (bool,_) = queryNodeWithPath source node []


traverseTrie source@(s:_)  node@TrieNode{ nodeType = Top, children=ch}
    = 3

queryNodeWithPath :: (Ord a) => [a] -> TrieNode a -> [[a]] -> (QueryMatchType,[[a]])
queryNodeWithPath source@(s:_)  node@TrieNode{ nodeType = Top, children=ch} acc
          | Map.member s ch = queryNodeWithPath source (ch ! s) acc
          | otherwise       = (AintNothing,acc)
queryNodeWithPath source node@TrieNode{ wordFragment = target, nodeType=nT, children=ch } acc = case matchData of
                          ExactMatchDatum {shared=source} 
                                          | nT == Word -> (IsWord, (target:acc))
                                          | otherwise  -> (IsPrefix,acc)
                          TargetIsSmallerDatum {suffixS= suffixStringOfSource@(s:_)}
                              | Map.member s ch -> queryNodeWithPath suffixStringOfSource (ch ! s) (target:acc )
                              | otherwise  -> (AintNothing,acc)
                          SourceIsSmallerDatum{} -> (IsPrefix, acc)
                          otherwise -> (AintNothing,acc)
                        where matchData = source =><= target


--countWordsUtil node@TrieNode{nodeType = Word} acc =  (acc + 1) + foldChildren
---------------------------------------------------------------------------
--  some test code
---------------------------------------------------------------------------

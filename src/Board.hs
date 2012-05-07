{-# LANGUAGE OverloadedStrings #-}
-- this language directive needs to be at the top
-- http://stackoverflow.com/questions/7670072/parsing-complicated-jsons-with-aeson

module Main where 


import BoardGenerator.TemplateParser
import BoggleSolver.WordFinder
import BoggleSolver.WordFinder
import qualified System as S
import Char as C

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL

instance ToJSON WNode where
  toJSON WNode{ ident=i, contents=c, multiplier=m} = 
      object [ "content" .= c,
               "coordinate" .= object [ "col" .= fst i, "row" .= snd i],
               "type" .= (show m) ]

data AllOfIt = AllOfIt { allwords::[String], board::[WNode]}

instance ToJSON AllOfIt where
    toJSON AllOfIt{allwords =w, board=b} = object [ "words" .= w,
                                                    "board" .= object [ "nodes" .= b,
                                                                        "dimensions" .= [100::Int,100::Int] ]
                                                  ] 

main = do 
  args <- S.getArgs 
  contents <- readFile (args !! 0)
  results <- allOfIt contents
  results2 <- allOfIt contents
  case results of 
    Left _ ->
        putStrLn "nothing"
    Right (board,wordz) ->
        do 
          putStrLn "found"
          mapM putStrLn wordz
          let all = AllOfIt{allwords=wordz,board=board}
          BL.putStrLn (encode all)
          return ()

module Main where 

import BoardGenerator.TemplateParser
import qualified System as S

main = do 
  args <- S.getArgs 
  contents <- readFile (args !! 0)
  allOfIt contents
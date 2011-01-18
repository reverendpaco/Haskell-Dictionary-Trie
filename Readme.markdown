# A Simple Boggle Solver

* cabal install binary
* git clone git@github.com:reverendpaco/Haskell-Dictionary-Trie.git
* cd ./Haskell-Dictionary-Trie/src
* ghc --make WordServer.hs
* ./WordServer  (listens on 9900)
* (on another terminal)  telnet localhost 9900
* type in letters "sdfoidsfuodsf" which will be wrapped into the closest approximate square, for example

sdfoidsfuodsf ->

s d f o
i d s f
u o d s
f


* hit return
* watch as the words in this board are returned to you
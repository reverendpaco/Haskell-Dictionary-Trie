# A Simple Boggle Solver

* cabal install binary
* git clone git@github.com:reverendpaco/Haskell-Dictionary-Trie.git
* cd ./Haskell-Dictionary-Trie/src

# Either (Server Mode)
* ghc --make WordServer.hs
* ./WordServer  (listens on 9900)
* (on another terminal)  telnet localhost 9900
* type in letters "SERSPATGLINESERSOO" which will be wrapped into the closest approximate square, for example

SERSPATGLINESERSOO ->		
<pre>
                S E R S
                P A T G
                L I N E
                S E R S
                O O
</pre>
* hit return
* watch as the words in this board are returned to you

# Or (Command Line)
* ghc --make WordSolver.hs
* ./WorldSolver SERSPATGLINESERSOO
* watch as the words in this board are printed out
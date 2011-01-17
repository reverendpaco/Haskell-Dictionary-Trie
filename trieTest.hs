module Trie.TrieTest
    where
import Trie.TrieUtility
import Trie.Trie


-- ttW1 =  (( "as", "aseed"),SourceIsSmallerDatum {pre = "as", suffixT = "eed"})
-- ttW2 =  (( "as" ,"as"),ExactMatchDatum {shared = "as"})
-- ttW3 =  (( "aseed", "as"),TargetIsSmallerDatum {pre = "as", suffixS = "eed"})
-- ttW4 =  (( "---", "as"),NoMatchDatum)
-- ttW5 =  (( "---aaa", "---zzz"),SharedPrefixDatum {shared = "---", suffixT = "zzz", suffixS = "aaa"})

-- runSuite (tests,f) = map f tests
-- runSuiteWithDebug (tests,f) = zip (map f tests) tests
-- tttSuite = ([ttW1,ttW2,ttW3,ttW4,ttW5],testFunction)
--            where testFunction ( (source,target) , match ) = match ==  source =><= target 
-- runTTTSuite = runSuite tttSuite
-- runTTTSuiteDebug = runSuiteWithDebug tttSuite

-- testCtxtNode = (nonWordNode "ssss"){ children=Map.empty}
-- runit tt  = insertNode testCtxtNode (snd tt)


testWords = ["rebate",
             "reborn",
             "realize",
             "real",
             "relied",
             "rebates",
             "reb",
             "relief",
             "realizes",
             "redder",
             "red"]
----

    

basicEnglishWords =["a",
              "able",
              "about",
              "account",
              "acid",
              "across",
              "act",
              "addition",
              "adjustment",
              "advertisement",
              "advertisements",
              "after",
              "again",
              "against",
              "agreement",
              "air",
              "all",
              "almost",
              "among",
              "amount",
              "amusement",
              "and",
              "angle",
              "angry",
              "animal",
              "answer",
              "ant",
              "any"]
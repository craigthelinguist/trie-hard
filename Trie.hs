
module Trie where

import Data.Map (Map)
import qualified Data.Map as Map

{-| A Trie is a data structure for storing and retrieving Strings fast. -}
data Trie = Trie { wordCount :: Int
                 , children :: Map Char Trie
                 } deriving (Show)


{-| emptyTrie :: Trie
    Returns the empty trie. -}
emptyTrie :: Trie

emptyTrie = Trie 0 Map.empty



{-| trieCount :: String -> Trie -> Int
    Count the number of times the specified string exists in the Trie. -}
trieCount :: String -> Trie -> Int

-- Empty string doesn't appear in the Trie.
trieCount [] t = 0

-- Recurse on nodes.
trieCount (c:str) t =
    let kids = children t
    in case Map.lookup c kids of
        -- No such path to leaf node.
        Nothing     ->  0
        -- Base case (at end of string): return wordcount of current node.
        -- Recursive case: return wordcount of child node.
        Just kid    ->  if null str
                        then (wordCount kid)
                        else trieCount str kid



{-| trieAdd :: String -> Trie -> Trie
    Add a string to the Trie. Returns the new, updated Trie. -}
trieAdd :: String -> Trie -> Trie

-- Adding empty string doesn't change the trie.
trieAdd [] t = t

-- Base case: at end of string.
trieAdd (c:[]) t =
    let kids = children t
    in case Map.lookup c kids of
        -- Need to create a new child node which is terminal.
        Nothing     ->  let kid' = Trie 1 Map.empty
                        in Trie (wordCount t) (Map.insert c kid' kids)
        -- Need to update existing child node.
        Just kid    ->  let kid' = Trie (succ . wordCount $ kid) (children kid)
                        in Trie (wordCount t) (Map.insert c kid' kids)

-- Recursive case: add to appropriate child and update yourself as you return.
trieAdd (c:str) t =
    let kids = children t
    in case Map.lookup c kids of
        -- No such child; make one and recurse on it. Update yourself after.
        Nothing     ->  let kid' = trieAdd str emptyTrie
                        in Trie (wordCount t) (Map.insert c kid' kids)
        -- Child exists; recurse on it and update yourself after.
        Just kid    ->  let kid' = trieAdd str kid
                        in Trie (wordCount t) (Map.insert c kid' kids)



{-| trieHas :: String -> Trie -> Bool
    Check if the Trie has the given String. -}
trieHas :: String -> Trie -> Bool
trieHas str trie = (trieCount str trie) > 0



{-| trieSize
    Count the number of unique strings in the Trie. -}
trieSize :: Trie -> Int
trieSize trie =
    let sizeOfThis = if (wordcount trie) > 0 then 1 else 0
        trieKids = Map.elems (children trie)
    in sizeOfThis + sum (map trieSize trieKids)

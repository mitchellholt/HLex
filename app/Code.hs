module Code (genCode, buildTree, Filetype (..), toLanguage) where

import Control.Monad.Trans.State

data Filetype = Haskell | Rust | C deriving (Show)

type SuffixTree = [SuffixTreeChild]

data SuffixTreeChild = Node Char [SuffixTreeChild] | Leaf String

data Code = Code
  { stateId :: Int,
    output :: Maybe String,
    transitions :: [(Char, Code)]
  }

instance Show Code where
  show c =
    let titleStr = "State " ++ (show . stateId) c ++ ":\n"
        outputStr = case output c of
          Nothing -> ""
          Just s -> "\tReturn" ++ s ++ "\n"
        transitionsStr =
          concatMap
            ( \(chr, code) ->
                "\tGo to "
                  ++ (show . stateId) code
                  ++ " on "
                  ++ show chr
                  ++ "\n"
            )
            (transitions c) -- empty iff outputStr is not empty
     in titleStr
          ++ outputStr
          ++ transitionsStr
          ++ "\n"
          ++ concatMap show (transitions c)

-- BEGIN -- David's bug ridden imperative code

buildTree :: [String] -> SuffixTree
buildTree = foldl (\x y -> insertToChildren y y x) []

buildToLeaf :: String -> String -> SuffixTreeChild
buildToLeaf "" og = Leaf og
buildToLeaf remaining og = Node (head remaining) [buildToLeaf (tail remaining) og]

insertToNode :: String -> String -> SuffixTreeChild -> SuffixTreeChild
insertToNode _ _ (Leaf x) = Leaf x
insertToNode remaining og (Node nodeChar children) =
  if nodeChar == head remaining
    then Node nodeChar (insertToChildren (tail remaining) og children)
    else Node nodeChar children

insertToChildren :: String -> String -> [SuffixTreeChild] -> [SuffixTreeChild]
insertToChildren remaining og children =
  if any (matchChar (head remaining)) children
    then map (insertToNode remaining og) children
    else buildToLeaf remaining og : children

matchChar :: Char -> SuffixTreeChild -> Bool
matchChar _ (Leaf _) = False
matchChar char (Node nodeChar _) = char == nodeChar

-- END -- David's bug ridden imperative code

genCode :: SuffixTree -> Code
genCode = fst . flip runState 0 . genCodeST

genCodeST :: SuffixTree -> State Int Code
genCodeST xs = do
  ident <- nextID
  childTransitions <- zip (getNodeChar <$> xs) <$> traverse genCodeST' xs -- if any leaves, error is here
  return
    Code
      { stateId = ident,
        transitions = childTransitions,
        output = Nothing
      }

genCodeST' :: SuffixTreeChild -> State Int Code
genCodeST' (Leaf s) = do
  ident <- nextID
  return Code {stateId = ident, transitions = [], output = Just s}
genCodeST' (Node _ children) = case children of
  [] -> error "Found node with no children"
  (Leaf s) : xs ->
    if null xs
      then genCodeST' (Leaf s)
      else error leafErrorMsg
  xs -> genCodeST xs

leafErrorMsg :: String
leafErrorMsg = "Found node with leaf and other children"

getNodeChar :: SuffixTreeChild -> Char
getNodeChar (Node c _) = c
getNodeChar _ = error leafErrorMsg

nextID :: State Int Int
nextID = get <* modify (+ 1)

toLanguage :: Filetype -> Code -> String -- TODO, last thing to work on
toLanguage = undefined

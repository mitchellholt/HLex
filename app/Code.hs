module Code (genCode, buildTree, Filetype(..), toLanguage) where

data Filetype = Haskell | Rust | C deriving Show

data SuffixTree = Node Char [SuffixTree] | Leaf

type Code = () -- todo make this


buildTree :: [String] -> SuffixTree
buildTree = undefined


genCode :: Filetype -> SuffixTree -> Code
genCode = undefined


toLanguage :: Filetype -> Code -> String
toLanguage = undefined

module Code (genCode, buildTree, Filetype(..), toLanguage) where

import Control.Monad.Trans.State

data Filetype = Haskell | Rust | C deriving Show

type SuffixTree = [SuffixTreeChild]
data SuffixTreeChild = Node Char [SuffixTreeChild] | Leaf String

data Code = Code
    { stateId :: Int
    , transitions :: [(Char, Code)]
    , output :: Maybe String
    }

instance Show Code where
    show = undefined -- TODO


-- Dave
buildTree :: [String] -> SuffixTree
buildTree = undefined


genCode :: SuffixTree -> Code
genCode = fst . flip runState 0 . genCodeST


genCodeST :: SuffixTree -> State Int Code
genCodeST xs = do
    ident <- nextID
    childTransitions <- zip (getNodeChar <$> xs) <$> traverse genCodeST' xs -- if any leaves, error is here
    return Code
        {stateId = ident
        , transitions = childTransitions
        , output = Nothing
        }


genCodeST' :: SuffixTreeChild -> State Int Code
genCodeST' (Leaf s) = do
    ident <- nextID
    return Code {stateId = ident, transitions = [], output = Just s}
genCodeST' (Node _ children) = case children of
        [] -> error "Found node with no children"
        (Leaf s):xs -> if null xs
            then genCodeST' (Leaf s)
            else error leafErrorMsg
        xs -> genCodeST xs


leafErrorMsg :: String
leafErrorMsg = "Found node with leaf and other children"


getNodeChar :: SuffixTreeChild -> Char
getNodeChar (Node c _) = c
getNodeChar _ = error leafErrorMsg

nextID :: State Int Int
nextID = get <* modify (+1)


toLanguage :: Filetype -> Code -> String
toLanguage = undefined

module CodeGenerator(genCode, Language(..)) where

import TreeGenerator (STree(..))
import Data.Char
import Snippits.Haskell

data Language = Haskell | Rust

data Code = Code { lexerBranches :: [Branch], tokens :: [String] }
data Branch = Branch { action :: Action, path :: String } deriving Show
data Action = Shift | Reduce String | TryReduce String deriving Show

-- for debugging
instance Show Code where
    show code = "Tokens: " ++ show (tokens code) ++ "\n"
        ++ (unlines . fmap show) (lexerBranches code)


-- make intermediate code for lexer then convert to actual language code with
-- data types for tokens, token stream, and functions for
genCode :: Language -> [String] -> [STree] -> String
genCode Haskell tokenNames = haskellGen . interCode tokenNames
genCode Rust tokenNames = rustGen . interCode tokenNames


-- Generate the intermediate code representing the lexer
interCode :: [String] -> [STree] -> Code
interCode tokenNames trees = Code
    { lexerBranches = concatMap (withPath "") trees, tokens = tokenNames }


withPath :: String -> STree -> [Branch]
withPath _ (Leaf _) = [] -- seems strange but is necessary
withPath p (Node c children)
    | length children == 1 && any isLeaf children =
        let
            Leaf tokenName = head children -- TODO get rid of warning
        in
            if isAlpha c then
                pure Branch { action = TryReduce tokenName, path = p ++ pure c }
            else
                pure Branch { action = Reduce tokenName, path = p ++ pure c }

    | length children > 1 && any isLeaf children =
        let
            Leaf tokenName = head . dropWhile (not . isLeaf) $ children -- TODO get rid of warning
        in
            Branch { action = TryReduce tokenName, path = p ++ pure c }
                : concatMap (withPath (p ++ pure c)) children
    | otherwise =
        Branch { action = Shift, path = p ++ pure c } : concatMap (withPath (p ++ pure c)) children
    where
        isLeaf :: STree -> Bool
        isLeaf (Leaf _) = True
        isLeaf _        = False


haskellGen :: Code -> String
haskellGen code = unlines $
    [
        header,
        makeTokenType (tokens code),
        apiDefs,
        fromType
    ]
    ++ (fromBranch <$> lexerBranches code)
    ++ pure fromFinal
    where
        fromBranch :: Branch -> String
        fromBranch branch = case action branch of
            Shift -> functionDef ++ shift
            Reduce tokenName -> functionDef ++ reduce tokenName
            TryReduce tokenName -> functionDef ++ "do\n"
                ++ tryReduce tokenName (path branch)
            where
                functionDef = "from " ++ show (path branch) ++ " = "


rustGen :: Code -> String
rustGen = undefined

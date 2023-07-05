module CodeGenerator(genCode, Language(..)) where

import TreeGenerator (STree(..))
import Snippits.Haskell

data Language = Haskell | Rust

data Code = Code { lexerBranches :: [Branch], tokens :: [String] }
data Branch = Branch { action :: Action, path :: String } deriving Show
-- reduce has a follow set; we can only reduce if the next character (if any)
-- is not in the follow set
data Action = Shift | Reduce String [Char] deriving Show

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
withPath _ (Leaf _) = [] -- need to do all calculation at parent rip
withPath p (Node c children)
    | any isLeaf children =
        let
            tokenName = getLeaf . head . dropWhile (not . isLeaf) $ children
            followSet = map getNode . filter (not . isLeaf) $ children
        in
            Branch { action = Reduce tokenName followSet, path = nextPath }
                : concatMap (withPath nextPath) children
    | otherwise = Branch { action = Shift, path = p ++ pure c }
        : concatMap (withPath nextPath) children
    where
        isLeaf :: STree -> Bool
        isLeaf (Leaf _) = True
        isLeaf _ = False

        getLeaf :: STree -> String
        getLeaf (Leaf s) = s
        getLeaf _ = undefined

        getNode :: STree -> Char
        getNode (Node ch _) = ch
        getNode _ = undefined

        nextPath = p ++ pure c


haskellGen :: Code -> String
haskellGen code = unlines $
    [
        header,
        makeTokenType (tokens code),
        apiDefs,
        fromInit
    ]
    ++ (fromBranch <$> lexerBranches code)
    ++ pure fromFinal
    where
        fromBranch :: Branch -> String
        fromBranch branch = case action branch of
            Shift -> functionDef ++ shift (path branch)
            Reduce tokenName followSet -> functionDef ++ "do\n"
                ++ reduce tokenName (path branch) followSet 
            where
                functionDef = "from " ++ show (path branch) ++ " = "


rustGen :: Code -> String
rustGen = undefined

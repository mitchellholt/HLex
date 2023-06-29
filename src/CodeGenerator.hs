module CodeGenerator(genCode, Language(..)) where

import TreeGenerator (STree(..))
import HaskellSnippits

data Language = Haskell | Rust

-- make intermediate code for lexer then convert to actual language code with
-- data types for tokens, token stream, and functions for
--  next :: Either EOFError Token 
-- and
--  match :: Either (CompError or EOFError) ()
genCode :: Language -> [String] -> [STree] -> String
genCode Haskell = haskellGen
genCode Rust = rustGen


haskellGen :: [String] -> [STree] -> String
haskellGen tokenNames trees = unlines
    [
        header,
        imports,
        tokenType tokenNames,
        typesAndInstances,
        apiFunctions,
        "",
        nextType,
        "next = from",
        "",
        genNextFnHaskell [] trees
    ]


genNextFnHaskell :: [Char] -> [STree] -> String
genNextFnHaskell path children = unlines
    [
        name ++ fnType,
        name ++ " = do",
        body
    ]
    ++ otherFunctions
    where
        name = fnName path
        fnType = " :: TokenStream Token"
        body = (unlines . fmap (indent 1))
            [
                "c <- char"
            ]
            ++ (unlines . fmap (indent 2 . rec path)) children
            ++ "backtrack " ++ path ++ " >> ident"
        -- TODO recursively generate all other necessary functions
        otherFunctions = undefined


indent :: Int -> String -> String
indent x = comp (4 * x) (' ' :)
    where
        comp :: Int -> (a -> a) -> (a -> a)
        comp n f = foldr (.) id (replicate n f)


-- the branche for what a character can be on the top node of an STree
rec :: [Char] -> STree -> String
rec _ (Leaf s) = "if isSpace c then return Token { tokenType = " ++ s ++ ", identifier = Nothing } else"
rec path (Node a _) = "if c == " ++ pure a ++ " then " ++ fnName (path ++ pure a) ++ " else"


-- TODO but first write a few examples because rust is hard -- do not use string
-- slices for identifiers because we want the caller of the API to own
-- everything they look at
rustGen :: [String] -> [STree] -> String
rustGen = undefined

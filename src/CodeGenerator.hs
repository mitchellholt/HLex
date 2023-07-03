module CodeGenerator(genCode, Language(..)) where

import TreeGenerator (STree(..))

data Language = Haskell | Rust

data Code

-- make intermediate code for lexer then convert to actual language code with
-- data types for tokens, token stream, and functions for
genCode :: Language -> [String] -> [STree] -> String
genCode Haskell tokenNames = haskellGen . interCode tokenNames
genCode Rust tokenNames = rustGen . interCode tokenNames


-- Generate the intermediate code representing the lexer
interCode :: [String] -> [STree] -> Code
interCode = undefined


haskellGen :: Code -> String
haskellGen = undefined


rustGen :: Code -> String
rustGen = undefined

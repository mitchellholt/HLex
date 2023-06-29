module HaskellSnippits where


header :: String
header = unlines
    [
        "{-# LANGUAGE LambdaCase #-}",
        "module Lexer(next, match, Token(..), TokenType(..), StreamError(..), STFail(..), TokenStream(..)) where"
    ]

imports :: String
imports = unlines
    [
        "import Prelude hiding (fail)",
        "import Data.Char",
        "import Control.Monad (void)"
    ]

tokenType :: [String] -> String
tokenType tokenNames = "data TokenType = Identifier"
    ++ concatMap (" | " ++) tokenNames ++ " deriving (Eq, Show)"

typesAndInstances :: String
typesAndInstances = unlines
    [
        "data Token = Token { tokenType :: TokenType, identifier :: Maybe String }",
        "",
        "data StreamError = EOFError | CompError { expected :: TokenType, actual :: TokenType } | InvalidSymbol Char",
        "",
        "-- State transformer which can fail -- e is error type, s is state type and",
        "-- a is value type",
        "newtype STFail e s a = STF { runstate :: s -> Either e (s, a) }",
        "",
        "type TokenStream = STFail StreamError String",
        "",
        "",
        "instance Show StreamError where",
        "    show EOFError = \"Error: unexpected EOF\"",
        "    show (CompError e act) = \"Error: expected token \" ++ show e",
        "        ++ \" but found \" ++ show act",
        "    show (InvalidSymbol c) = \"Error: bad symbol \" ++ pure c",
        "",
        "instance Show Token where",
        "    show token = case identifier token of",
        "    Nothing -> (show . tokenType) token",
        "    Just s -> \"IDENT(\" ++ s ++ \")\"",
        "",
        "",
        "instance Functor (STFail e s) where",
        "    fmap f st = STF { runstate = fmap (fmap f) . runstate st }",
        "",
        "instance Applicative (STFail e s) where",
        "    pure x = STF { runstate = Right . (, x) }",
        "    stf <*> sta = STF { runstate = \\s -> case runstate stf s of",
        "        Left e -> Left e",
        "        Right (s', f) -> runstate (f <$> sta) s'",
        "    }",
        "",
        "instance Monad (STFail e s) where",
        "    sta >>= f = STF { runstate = \\s -> case runstate sta s of",
        "        Left e -> Left e",
        "        Right (s', a) -> runstate (f a) s'",
        "    }"
    ]


apiFunctions :: String
apiFunctions = unlines
    [
        "fail :: StreamError -> TokenStream a",
        "fail e = STF { runstate = const (Left e) }",
        "",
        "",
        "match :: TokenType -> TokenStream Token",
        "match t = do",
        "    token <- next",
        "    if tokenType token == t then return token",
        "        else fail (CompError { expected = t, actual = tokenType token })",
        "",
        "",
        "char :: TokenStream Char",
        "char = STF { runstate = \\case",
        "    [] -> Left EOFError",
        "    (c:cs) -> Right (cs, c)",
        "}",
        "",
        "",
        "while :: (Char -> Bool) -> TokenStream String",
        "while p = STF { runstate = \\case",
        "    [] -> Right ([], [])",
        "    (c:cs) -> if p c then runstate ((c :) <$> while p) cs",
        "        else Right (c:cs, [])",
        "}",
        "",
        "",
        "blank :: TokenStream ()",
        "blank = void (while isSpace)",
        "",
        "",
        "modify :: (String -> String) -> TokenStream ()",
        "modify f = STF { runstate = Right . (, ()) . f }",
        "",
        "",
        "backtrack :: String -> TokenStream ()",
        "backtrack = modify . (++)",
        "",
        "",
        "charIf :: (Char -> Bool) -> TokenStream Char",
        "charIf p = do",
        "    c <- char",
        "    if p c then pure c else fail (InvalidSymbol c)",
        "",
        "",
        "ident :: TokenStream Token",
        "ident = do",
        "    c <- charIf (`elem` ['a'..'z'])",
        "    rest <- identRest",
        "    return Token { tokenType = Identifier, identifier = Just (c : rest) }",
        "",
        "",
        "identRest :: TokenStream String",
        "identRest = STF { runstate = \\case",
        "    [] -> Right ([], [])",
        "    (c:cs)",
        "        | isSpace c",
        "            -> Right (c:cs, [])",
        "        | elem c (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])",
        "            -> runstate ((c :) <$> identRest) cs",
        "        | otherwise",
        "            -> Left (InvalidSymbol c)",
        "}"
    ]


nextType :: String
nextType = "next :: TokenStream Token"


fnName :: [Char] -> String
fnName path = "from" ++ path

module Snippits.Haskell where

import Snippits.Utils
import Data.Char

header :: String
header = unlines
    [
        "{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}",
        "{-# LANGUAGE LambdaCase #-}",
        "",
        "module Lexer(next,",
        "             match,",
        "             streamFail,",
        "             endOfStream,",
        "             peek,",
        "             Token(..),",
        "             TokenType(..),",
        "             StreamError(..),",
        "             STFail(..),",
        "             TokenStream) where",
        "",
        "import Data.Char",
        "import Control.Monad (void)"
    ]


-- Generate the enum tyoe definition for 
makeTokenType :: [String] -> String
makeTokenType tokenNames = "data TokenType = Identifier\n"
    ++ (unlines . map (indent n . (++) "| ")) ("Number" : tokenNames)
    ++ indent n "deriving (Eq, Show)"
    where
        n = length "data TokenType "


apiDefs :: String
apiDefs = unlines
    [
        "data Token = Token { tokenType :: TokenType, value :: Maybe String }",

        "data StreamError = EOFError",
        "                 | CompError { expected :: TokenType, actual :: TokenType }",
        "                 | InvalidSymbol Char",
        "                 | UnexpectedToken TokenType",
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
        "    show (UnexpectedToken t) = \"Error: unexpected token \" ++ show t",
        "",
        "instance Show Token where",
        "    show token = case value token of",
        "        Nothing -> (show . tokenType) token",
        "        Just s -> \"IDENT(\" ++ s ++ \")\"",
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
        "    }",
        "",
        "",
        "-- Fail inside the TokenStream monad",
        "streamFail :: StreamError -> TokenStream a",
        "streamFail e = STF { runstate = const (Left e) }",
        "",
        "",
        "endOfStream :: TokenStream Bool",
        "endOfStream = STF { runstate = \\case",
        "    [] -> Right ([], True)",
        "    cs -> Right (cs, False)",
        "}",
        "",
        "",
        "-- Transform a TokenStream monad into one which fails if the predicate fails",
        "guard :: TokenStream a -> (a -> Bool) -> (a -> StreamError) -> TokenStream a",
        "guard stream p err = do",
        "    a <- stream",
        "    if p a then return a else streamFail (err a)",
        "",
        "",
        "-- match given token with the next",
        "match :: TokenType -> TokenStream Token",
        "match t = guard next ((== t) . tokenType) (CompError t . tokenType)",
        "",
        "",
        "-- consume a single character from input",
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
        "lookahead :: TokenStream (Maybe Char)",
        "lookahead = STF { runstate = \\case",
        "    [] -> Right ([], Nothing)",
        "    (c:cs) -> Right (c:cs, Just c)",
        "}",
        "",
        "",
        "",
        "blank :: TokenStream ()",
        "blank = void (while isSpace)",
        "",
        "",
        "modify :: (String -> String) -> TokenStream String",
        "modify f = STF { runstate = \\s -> Right (f s, f s) }",
        "",
        "",
        "backtrack :: String -> TokenStream ()",
        "backtrack = void . modify . (++)",
        "",
        "",
        "identifier :: TokenStream Token",
        "identifier = do",
        "    v <- lookahead",
        "    case v of",
        "        Nothing -> streamFail EOFError",
        "        Just c -> if isAlpha c then ident",
        "                               else if isDigit c then number",
        "                               else streamFail (InvalidSymbol c)",
        "",
        "",
        "ident :: TokenStream Token",
        "ident = do",
        "    word <- while (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']))",
        "    void (guard lookahead f err)",
        "    return Token { tokenType = Identifier, value = Just word }",
        "    where",
        "        f :: Maybe Char -> Bool",
        "        f Nothing = False",
        "        f (Just c) = isSpace c",
        "",
        "        err :: Maybe Char -> StreamError",
        "        err Nothing = EOFError",
        "        err (Just c) = InvalidSymbol c",
        "",
        "",
        "number :: TokenStream Token",
        "number = do",
        "    n <- while isDigit",
        "    v <- lookahead",
        "    case v of",
        "        Just '.' -> do",
        "            void char",
        "            rest <- while isDigit",
        "            return Token { tokenType = Number, value = Just (n ++ ('.' : rest)) }",
        "        _        -> return Token { tokenType = Number, value = Just n }",
        "",
        "",
        "next :: TokenStream Token",
        "next = blank >> from \"\"",
        "",
        "",
        "canReduce :: Maybe Char -> [Char] -> Bool",
        "canReduce Nothing _ = True",
        "canReduce (Just c) followSet = not (elem c followSet)",
        "",
        "",
        "alphaNum :: [Char]",
        "alphaNum = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']",
        "",
        "",
        "peek :: TokenStream Token",
        "peek = do",
        "    str <- modify id",
        "    tok <- next",
        "    _ <- modify (const str)",
        "    return tok"
    ]


shift :: String
shift = "char >>= from . (++) \"\" . pure"


reduce :: String -> String -> [Char] -> String
reduce tokenName path followSet =
    let
        followSet' = if all (\x -> isAlpha x || isNumber x) tokenName
            then "(" ++ show followSet ++ " ++ alphaNum)"
            else show followSet
    in
        unlines . map (indent 4) $
            [
                "v <- lookahead",
                "if canReduce v " ++ followSet' ++ " then return Token { tokenType = " ++ tokenName ++ ", value = Nothing }",
                "else char >>= from . (++) " ++ show path ++ " . pure"
            ]


fromType :: String
fromType = "from :: String -> TokenStream Token"


fromFinal :: String
fromFinal = "from str = backtrack str >> identifier"


alphaNum :: String
alphaNum = "alphaNum"

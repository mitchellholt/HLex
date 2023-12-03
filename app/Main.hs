module Main where

import System.Environment
import System.Exit
import Control.Monad (when)
import Code

data Args = Args
    { inFile :: String
    , outFile :: String
    , fileType :: Filetype
    , debugOutput :: Bool
    } deriving Show


main :: IO ()
main = do
    parsedArgs <- processArgs <$> getArgs
    case parsedArgs of
        Left errStr -> putStrLn errStr >> exitWith (ExitFailure 1)
        Right args -> do
            print args
            code <- genCode . buildTree . lines <$> readFile (inFile args)
            when (debugOutput args) (print code)
            writeFile (outFile args) (toLanguage (fileType args) code)
            exitSuccess


usageErrStr :: String
usageErrStr = "Usage: fsm [-g, --debug] <input file> <output file>"


-- Check that the two mandatory arguments inFile, outFile (in that order) are
--  included, also check for a debug flag (-g or --debug) which will determine
--  if the intermediate language is printed to stdout. The fileType will be
--  determined by parsing fileType into the format <name>.<type>
-- If for any reason parsing fails, create return an appropriate error string
processArgs :: [String] -> Either String Args
processArgs args = do
    let args' = tail args
    allValidArgs args'
    inFileName <- getInFileName args'
    outFileName <- getOutFileName args'
    language <- (toFiletype . last . split '.') outFileName
    return Args
        {inFile = inFileName
        , outFile = outFileName
        , fileType = language
        , debugOutput = elem "-g" args' || elem "--debug" args'
        }


allValidArgs :: [String] -> Either String ()
allValidArgs args
    | null badFlags = pure ()
    | otherwise = Left ("Unknown flag: " ++ head badFlags)
    where
        badFlags = filter (\s -> head s == '-' && s /= "-g" && s /= "--debug") args


getInFileName :: [String] -> Either String String
getInFileName args = case dropWhile ((== '-') . head) args of
    [] -> Left usageErrStr
    (s:_) -> Right s


getOutFileName :: [String] -> Either String String
getOutFileName args = case dropWhile ((== '-') . head) args of
    [] -> Left usageErrStr
    (_:rest) -> case dropWhile ((== '-') . head) rest of
        [] -> Left usageErrStr
        (s:_) -> Right s


split :: Char -> String -> [String]
split c str =
    let
        start = takeWhile (/= c) str
        rest = dropWhile (/= c) str
    in case rest of
        [] -> [start]
        (_:end) -> start : split c end


toFiletype :: String -> Either String Filetype
toFiletype str
    | str == "hs" = Right Haskell
    | str == "rs" = Right Rust
    | str == "c" = Right C
    | otherwise = Left "File extension not supported - must be hs, rs, or c"

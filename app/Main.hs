module Main where

import System.Environment
import Data.Void (Void)

data Filetype = Haskell | Rust | C deriving Show

data Args = Args
    { inFile :: String
    , outFile :: String
    , fileType :: Filetype
    , debugOutput :: Bool
    } deriving Show

data SuffixTree = Node Char [SuffixTree] | Leaf Char

type Code = Void -- todo make this


main :: IO ()
main = do
    parsedArgs <- processArgs <$> getArgs
    case parsedArgs of
        Left errStr -> putStrLn errStr
        Right args -> do
            print args
            code <- genCode (fileType args) . buildTree . lines <$> readFile (inFile args)
            print code


-- Check that the two mandatory arguments inFile, outFile (in that order) are
--  included, also check for a debug flag (-g or --debug) which will determine
--  if the intermediate language is printed to stdout. The fileType will be
--  determined by parsing fileType into the format <name>.<type>
-- If for any reason parsing fails, create return an appropriate error string
processArgs :: [String] -> Either String Args
processArgs = undefined


buildTree :: [String] -> SuffixTree
buildTree = undefined


genCode :: Filetype -> SuffixTree -> Code
genCode = undefined


module Main where

import qualified Data.Map as Map
import Data.Char (isSpace)
import System.Directory (doesFileExist)
import TreeGenerator (STree, makeTree)
import CodeGenerator (genCode, Language(..))


main :: IO ()
main = do
    -- TODO get an output language from cmd line; Haskell is hard-coded for now
    -- file format is {TOKEN_NAME token_definition\n}
    exists <- doesFileExist "tokens.hlex"
    if exists then do
        contents <- filter (not . blank) . lines <$> readFile "tokens.hlex"
        case buildMap contents of
            Left e -> putStrLn e
            Right tokenMap -> case makeTree tokenMap of
                Left xs -> foldr ((>>) . putStrLn) (pure ()) xs
                Right forest -> (putStrLn . genCode Haskell (Map.keys tokenMap)) forest
    else putStrLn "Cannot find file \"tokens.hlex\""


buildMap :: [String] -> Either String (Map.Map String String)
buildMap [] = pure Map.empty
buildMap (x:xs) = do
    let name = (takeWhile (not . isSpace) . dropWhile isSpace) x
    let definition = (dropWhile isSpace . dropWhile (not . isSpace)) x
    m <- buildMap xs
    case m Map.!? name of
        Nothing -> if null definition
            then Left ("Empty definition of token " ++ show name)
            else return (Map.insert name definition m)
        Just _ -> Left ("Duplicate definition of token " ++ show name)


blank :: String -> Bool
blank str = all isSpace str || take 2 str == "//"

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
module TreeGenerator(STree(..), makeTree) where

import qualified Data.Map as Map
import Data.Foldable

type TokenMap = Map.Map String String -- Map Token Definition

data STree = Node Char [STree] | Leaf String deriving Show


makeTree :: TokenMap -> Either [String] [STree]
makeTree = traverse checkTree . tokenTree


-- check there is no ambiguity -- this happens iff no node has more than one
-- leaf
checkTree :: STree -> Either [String] STree
checkTree (Leaf s) = pure (Leaf s)
checkTree (Node c children)
    | length leaves >= 2
        = Left ("Ambiguous definitions for tokens" : (fromLeaf <$> leaves))
    | otherwise
        = traverse_ checkTree children >> return (Node c children)
    where
        leaves = filter isLeaf children
        isLeaf (Leaf _) = True
        isLeaf (Node _ _) = False
        fromLeaf (Leaf s) = s
        fromLeaf _ = undefined



-- build a tree of all tokens
-- Require that the token map has no duplicates
tokenTree :: TokenMap -> [STree]
tokenTree m
    | Map.null m = []
    | otherwise  = do
        ch <- firstCharacters m
        case ch of
            Left token -> return (Leaf token)
            Right c ->  let f = fmap tail
                                . Map.filter ((== c) . head)
                                . Map.filter (not . null)
                        in pure (Node c (tokenTree (f m)))


firstCharacters :: TokenMap -> [Either String Char]
firstCharacters = unique . fmap f . Map.toList
    where
        f (k, v) = if null v then Left k else Right (head v)


unique :: Eq a => [a] -> [a]
unique = foldr (\x xs -> if elem x xs then xs else x:xs) []

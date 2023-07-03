module Snippits.Utils where


-- indent a string by padding it with n spaces
indent :: Int -> (String -> String)
indent n = foldr (.) id (replicate n (' ' :))

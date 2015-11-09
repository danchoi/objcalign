module Main where
import Data.List (elemIndex)




-- Data.List elemIndex :: Eq a => a -> [a] -> Maybe Int



alignMethods :: String -> String
alignMethods s = 
    let xs              = lines s
        maxIndent = foldr maxColonIndent 0 xs 
        xs' = map (reIndent maxIndent) xs
    in unlines xs'

reIndent :: Int -> String -> String
reIndent 0 s = s
reIndent n s | colonIndex s == 0  = s
             | n > (colonIndex s) = addSpaces (n - (colonIndex s)) s
             | n < (colonIndex s) = removeSpaces ((colonIndex s) - n) s
reIndent _ s = s

addSpaces n s = (take n $ repeat ' ') ++ s

-- Prelude dropWhile :: (a -> Bool) -> [a] -> [a]
removeSpaces n s = 
    let s' = dropWhile (== ' ') s
    in addSpaces n s'

maxColonIndent :: String -> Int -> Int 
maxColonIndent s oldMax = max oldMax (colonIndex s) 

colonIndex :: String -> Int
colonIndex s = maybe 0 id (elemIndex ':' s)

-- Prelude foldr :: (a -> b -> b) -> b -> [a] -> b

-- Data.List foldl' :: (a -> b -> a) -> a -> [b] -> a

main =  do 
  interact alignMethods
    

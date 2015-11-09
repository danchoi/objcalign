module Main where
import Data.List (elemIndex)




-- Data.List elemIndex :: Eq a => a -> [a] -> Maybe Int



alignMethods :: String -> String
alignMethods s = 
    let xs              = lines s
        firstIndent = colonIndex (head xs)
        i = foldr minIndent 0 xs 
        i' = max firstIndent i
        xs' = map (reIndent i') xs
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

minIndent :: String -> Int -> Int 
minIndent s old = min old (colonIndex s) 

colonIndex :: String -> Int
colonIndex s = maybe 0 id (elemIndex ':' s)

-- Prelude foldr :: (a -> b -> b) -> b -> [a] -> b

-- Data.List foldl' :: (a -> b -> a) -> a -> [b] -> a

main =  do 
  interact alignMethods
    

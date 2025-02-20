module H99 where

-- Problem 1
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (_:xs) = myLast xs

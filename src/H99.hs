module H99 where

-- https://ninetynine.haskell.chungyc.org/Problems.html#t:NestedList
data NestedList a
  = Elem a
  | List [NestedList a]
  deriving (Eq, Show)

-- https://ninetynine.haskell.chungyc.org/Problems.html#t:Encoding
data Encoding a
  = Single a
  | Multiple Int a
  deriving (Eq, Show)

-- Problem 1
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (_ : xs) = myLast xs

-- Problem 2
myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast [_] = Nothing
myButLast [x, _] = Just x
myButLast (_ : xs) = myButLast xs

-- Problem 3
elementAt :: [a] -> Int -> Maybe a
elementAt xs index = elementAt' xs index 1
  where
    elementAt' [] _ _ = Nothing
    elementAt' (x : xs') i c
      | i == c = Just x
      | otherwise = elementAt' xs' i (c + 1)

-- Problem 4
myLength :: [a] -> Int
myLength xs = myLength' xs 0
  where
    myLength' [] c = c
    myLength' (_ : xs') c = myLength' xs' (c + 1)

-- Problem 5
myReverse :: [a] -> [a]
myReverse xs = myReverse' xs []
  where
    myReverse' [] acc = acc
    myReverse' (x : xs') acc = myReverse' xs' (x : acc)

-- or
-- myReverse = foldr (\x -> (++ [x])) []

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = myReverse xs == xs

-- Problem 7
flatten :: NestedList a -> [a]
flatten a = flatten' a []
  where
    flatten' a' acc = case a' of
      Elem x -> x : acc
      List [] -> acc
      List [x] -> flatten' x acc
      List (x : xs) -> flatten' x acc ++ flatten' (List xs) acc

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x : xs) = compress' xs x [x]
  where
    compress' [] _ acc = acc
    compress' (x' : xs') lstel acc
      | x' == lstel = compress' xs' lstel acc
      | otherwise = compress' xs' x' (acc ++ [x'])

-- Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x : xs) = pack' xs x [x] []
  where
    pack' [] _ acc acc' = acc' ++ [acc]
    pack' (x' : xs') lstel acc acc'
      | x' == lstel = pack' xs' lstel (acc ++ [x']) acc'
      | otherwise = pack' xs' x' [x'] (acc' ++ [acc])

-- Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode a = encode' (pack a) []
  where
    encode' [] acc = acc
    encode' (x : xs) acc = encode' xs (acc ++ [(length x, head x)])

-- Problem 11
encodeModified :: (Eq a) => [a] -> [Encoding a]
encodeModified a = encodeModified' (encode a) []
  where
    encodeModified' [] acc = acc
    encodeModified' ((x, y) : xs) acc
      | x == 1 = encodeModified' xs (acc ++ [Single y])
      | otherwise = encodeModified' xs (acc ++ [Multiple x y])

-- Problem 12
decodeModified :: [Encoding a] -> [a]
decodeModified ea = decodeModified' ea []
  where
    decodeModified' [] acc = acc
    decodeModified' (x : xs) acc = case x of
      Single v -> decodeModified' xs (acc ++ [v])
      Multiple n v -> decodeModified' xs (acc ++ replicate n v)

-- Problem 13
encodeDirect :: (Eq a) => [a] -> [Encoding a]
encodeDirect [] = []
encodeDirect (x : xs) = encodeDirect' xs x 1 []
  where
    encodeDirect' [] lstel count acc
      | count == 1 = acc ++ [Single lstel]
      | count > 1 = acc ++ [Multiple count lstel]
      | otherwise = acc
    encodeDirect' (x' : xs') lstel count acc
      | x' /= lstel =
          if count == 1
            then encodeDirect' xs' x' 1 (acc ++ [Single lstel])
            else encodeDirect' xs' x' 1 (acc ++ [Multiple count lstel])
      | otherwise = encodeDirect' xs' lstel (count + 1) acc

-- Problem 14
dupli :: [a] -> [a]
dupli as = dupli' as []
  where
    dupli' [] acc = acc
    dupli' (x : xs) acc = dupli' xs (acc ++ [x, x])

-- Problem 15
repli :: [a] -> Int -> [a]
repli as n = repli' as n []
  where
    repli' [] _ acc = acc
    repli' (x : xs) n' acc = repli' xs n' (acc ++ replicate n' x)

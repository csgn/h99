module Main (main) where

import H99
import Test.HUnit

-- Test cases
-- Problem 1
testMyLast :: Test
testMyLast = TestCase $ do
  assertEqual "should be Just 4" (Just (4 :: Int)) (myLast [1, 2, 3, 4])
  assertEqual "should be Just 'z'" (Just ('z' :: Char)) (myLast ['x', 'y', 'z'])
  assertEqual "should be Nothing" (Nothing :: Maybe Int) (myLast [])

-- Problem 2
testMyButLast :: Test
testMyButLast = TestCase $ do
  assertEqual "should be Just 3" (Just (3 :: Int)) (myButLast [1, 2, 3, 4])
  assertEqual "should be Just 'y'" (Just ('y' :: Char)) (myButLast ['a' .. 'z'])
  assertEqual "should be Nothing" (Nothing :: Maybe Char) (myButLast ['a'])

-- Problem 3
testElementAt :: Test
testElementAt = TestCase $ do
  assertEqual "should be Just 2" (Just (2 :: Int)) (elementAt [1, 2, 3] 2)
  assertEqual "should be Just 'e'" (Just ('e' :: Char)) (elementAt "haskell" 5)
  assertEqual "should be Nothing" (Nothing :: Maybe Int) (elementAt [1, 2] 3)

-- Problem 4
testMyLength :: Test
testMyLength = TestCase $ do
  assertEqual "should be 3" 3 (myLength ([123, 456, 789] :: [Int]))
  assertEqual "should be 13" 13 (myLength "Hello, world!")

-- Problem 5
testMyReverse :: Test
testMyReverse = TestCase $ do
  assertEqual "should be reverse the string" "!amanap ,lanac a ,nalp a ,nam A" (myReverse "A man, a plan, a canal, panama!")
  assertEqual "should be reverse the list" [4, 3, 2, 1] (myReverse ([1, 2, 3, 4] :: [Int]))

-- Problem 6
testIsPalindrome :: Test
testIsPalindrome = TestCase $ do
  assertEqual "should be False" False (isPalindrome ([1, 2, 3] :: [Int]))
  assertEqual "should be True" True (isPalindrome "madamimadam")
  assertEqual "should be True" True (isPalindrome ([1, 2, 4, 8, 16, 8, 4, 2, 1] :: [Int]))

-- Problem 7
testFlatten :: Test
testFlatten = TestCase $ do
  assertEqual "should be [5]" ([5] :: [Int]) (flatten $ Elem 5)
  assertEqual "should be [1,2,3,4,5]" ([1, 2, 3, 4, 5] :: [Int]) (flatten $ List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
  assertEqual "should be []" ([] :: [Int]) (flatten $ List [])

-- Problem 8
testCompress :: Test
testCompress = TestCase $ do
  assertEqual "should be abcade" "abcade" (compress "aaaabccaadeeee")

-- Problem 9
testPack :: Test
testPack = TestCase $ do
  assertEqual "should be pack" (["aaaa", "b", "cc", "aa", "d", "eeee"] :: [String]) (pack "aaaabccaadeeee")

-- Problem 10
testEncode :: Test
testEncode = TestCase $ do
  assertEqual "should be encode" [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')] (encode "aaaabccaadeeee")

-- Problem 11
testEncodeModified :: Test
testEncodeModified = TestCase $ do
  assertEqual "should be encode" [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'] (encodeModified "aaaabccaadeeee")

-- Problem 12
testDecodeModified :: Test
testDecodeModified = TestCase $ do
  assertEqual "should be decode" "aaaabccaadeeee" (decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'])

-- Problem 13
testEncodeDirect :: Test
testEncodeDirect = TestCase $ do
  assertEqual "should be encode direct" [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'] (encodeDirect "aaaabccaadeeee")

-- Problem 14
testDupli :: Test
testDupli = TestCase $ do
  assertEqual "should be duplicate" ([1, 1, 2, 2, 3, 3] :: [Int]) (dupli [1, 2, 3])

-- Problem 15
testRepli :: Test
testRepli = TestCase $ do
  assertEqual "should be replicate" "aaabbbccc" (repli "abc" 3)

-- Group tests
tests :: Test
tests =
  TestList
    [ TestLabel "Last element of a list" testMyLast,
      TestLabel "Penultimate element of a list" testMyButLast,
      TestLabel "Find element of a list at a given position" testElementAt,
      TestLabel "Length of a list" testMyLength,
      TestLabel "Reverse a list" testMyReverse,
      TestLabel "Palindromes" testIsPalindrome,
      TestLabel "Flatten a nested list structure" testFlatten,
      TestLabel "Eliminate duplicate elements in a list" testCompress,
      TestLabel "Pack duplicates in a list" testPack,
      TestLabel "Run-length encoding of a list" testEncode,
      TestLabel "Modified run-length encoding" testEncodeModified,
      TestLabel "Decode a run-length encoded list" testDecodeModified,
      TestLabel "Run-length encoding of a list; direct solution" testEncodeDirect,
      TestLabel "Duplicate elements in a list" testDupli,
      TestLabel "Replicate elements of a list" testRepli
    ]

main :: IO ()
main = do
  counts' <- runTestTT tests
  if errors counts' + failures counts' == 0
    then putStrLn "All tests passed! ✅"
    else putStrLn "Some tests failed! ❌"

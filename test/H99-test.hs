module Main (main) where

import Test.HUnit
import H99

-- Test cases
-- Problem 1
testMyLast :: Test
testMyLast = TestCase $ do
    assertEqual "should be Just 4" (Just (4::Int)) (myLast [1, 2, 3, 4])
    assertEqual "should be Just 'z'" (Just ('z'::Char)) (myLast ['x', 'y', 'z'])
    assertEqual "should be Nothing" (Nothing:: Maybe Int) (myLast [])

-- Group tests
tests :: Test
tests = TestList [TestLabel "Last element of a list" testMyLast]


main :: IO ()
main = do
    counts' <- runTestTT tests
    if errors counts' + failures counts' == 0
        then putStrLn "All tests passed! ✅"
        else putStrLn "Some tests failed! ❌"

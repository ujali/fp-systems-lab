module Main where

-- Simple test framework without external dependencies
data TestResult = Pass | Fail String

testBasicArithmetic :: TestResult
testBasicArithmetic = 
  if 2 + 2 == 4
    then Pass
    else Fail "2 + 2 should equal 4"

testListOperations :: TestResult
testListOperations = 
  if map (*2) [1,2,3,4,5] == [2,4,6,8,10]
    then Pass
    else Fail "map doubles list"

testHigherOrderFunctions :: TestResult
testHigherOrderFunctions = 
  if foldr (+) 0 [1,2,3,4,5] == 15
    then Pass
    else Fail "foldr sums list"

testFunctionComposition :: TestResult
testFunctionComposition = 
  if ((*2) . (+1) $ 5) == 12
    then Pass
    else Fail "function composition"

allTests :: [(String, TestResult)]
allTests = 
  [ ("Basic Arithmetic", testBasicArithmetic)
  , ("List Operations", testListOperations)
  , ("Higher Order Functions", testHigherOrderFunctions)
  , ("Function Composition", testFunctionComposition)
  ]

runTest :: (String, TestResult) -> IO Bool
runTest (name, Pass) = do
  putStrLn $ "✓ " ++ name
  return True
runTest (name, Fail msg) = do
  putStrLn $ "✗ " ++ name ++ ": " ++ msg
  return False

runTests :: [(String, TestResult)] -> IO ()
runTests tests = do
  putStrLn "Running tests...\n"
  results <- mapM runTest tests
  let passed = length $ filter id results
  let total = length results
  putStrLn $ "\n" ++ show passed ++ " of " ++ show total ++ " tests passed"
  if passed == total
    then putStrLn "All tests passed!"
    else error "Some tests failed!"

main :: IO ()
main = runTests allTests

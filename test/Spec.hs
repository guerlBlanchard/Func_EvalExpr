import EvalExpr
import Parse
import Test.HUnit

main :: IO Counts
main = runTestTT myTest

myTest :: Test
myTest = test [ assertEqual "Basic Test of right parserChar" (Just ('a',"bc")) (runParser (parseChar 'a') "abc"),
                assertEqual "Basic Test of parserChar returning Nothing" (Nothing) (runParser (parseChar 'z') "abcd"),
                assertEqual "Basic Test of right parserAnyChar" (Just ('a',"bcd")) (runParser (parseAnyChar "bca") "abcd"),
                assertEqual "Basic Test of parserAnyChar returning Nothing" (Nothing) (runParser (parseAnyChar "xyz") "abcd")]
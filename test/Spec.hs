
import Test.Tasty
import qualified ParserSpec as PSpec

main :: IO ()
main = defaultMain tests

tests = testGroup "Tests" [PSpec.unitTests]

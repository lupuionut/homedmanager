import Test.HUnit
import Cmd

main :: IO Counts
main = runTestTT tests

tests = TestList [
            TestLabel "Configuration param found" test1,
            TestLabel "Empty configuration parameter" test2
        ]


test1 = TestCase (assertEqual "Configuration param found" (Just "/home") (Cmd.find "--config" ["--config","/home","this","is"]))
test2 = TestCase (assertEqual "Empty configuration parameter" (Nothing) (Cmd.find "--config" ["--config"]))

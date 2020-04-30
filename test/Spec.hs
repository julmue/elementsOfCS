import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit

import Logic.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ logicTests
    ]

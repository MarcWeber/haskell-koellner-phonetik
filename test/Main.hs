module Main where
import Control.Monad (when)
import System.Exit (exitFailure)
import Test.HUnit
import KoellnerPhonetik

createTest str code = str ~: (soundex_ger str) ~?= code

main = do
  (Counts cases tried errors failures) <- runTestTT $ TestList $ [
      createTest "Mueller" "657",
      createTest "Müller" "657",
      createTest "Meyer" "67",
      createTest "Meier" "67",
      createTest "Maier" "67",
      createTest "Maier" "67",
      createTest "Müller-Lüdenscheidt" "65752682",
      createTest "Zumpe" "861"
    ]

  when (errors + failures > 0) $ exitFailure

import Control.Monad
import System.Exit (exitFailure)
import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import qualified Test.Numeric.Tensile.Dimensions.Dims.Property as D
import qualified Test.Numeric.Tensile.Dimensions.Idxs.Property as I

tests :: IO [Bool]
tests = sequence [D.tests, I.tests]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- tests

  unless (and results) exitFailure

import Control.Monad
import System.Exit (exitFailure)
import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import qualified Test.Numeric.Tensile.Dimensions.Index.Property as DI
import qualified Test.Numeric.Tensile.Operations.Linear.Property as OL

tests = [DI.tests, OL.tests]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence tests :: IO [Bool]

  unless (and results) exitFailure

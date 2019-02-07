import Control.Monad
import System.Exit (exitFailure)
import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import qualified Test.Numeric.Tensile.Operations.Linear.Property as Linear


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence [Linear.tests] :: IO [Bool]

  unless (and results) exitFailure

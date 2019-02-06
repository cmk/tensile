import Control.Monad
import System.Exit (exitFailure)
import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import qualified Hedgehog.Gen as G
import Numeric.Tensile.Types
import Test.Data.Tensor.Generators

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  s <- G.sample (tensor (dims @_ @'[2,2]) rf)
  print s
  results <- sequence [] :: IO [Bool]

  unless (and results) exitFailure

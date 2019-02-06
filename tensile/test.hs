import Control.Monad
import System.Exit (exitFailure)
import System.IO (BufferMode(..), hSetBuffering, stdout, stderr)

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R
import Numeric.Tensile.Types
import Test.Numeric.Tensile.Tensor.Gen

rf :: Range Float
rf = R.linearFracFrom 0 (-100) 100

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  s <- G.sample (tensor (dims @_ @'[2,2]) G.float rf)
  print s
  results <- sequence [] :: IO [Bool]

  unless (and results) exitFailure

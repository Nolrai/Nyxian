module Main where

import Nyxian.Types
import Nyxian.Gen (Gen(..))
import Nyxian.Util (MonadRandom')

main :: IO ()
main = do
  [seed'] <- getArgs
  let seed = read seed'
  let profile = LangProfile
        { pOnsetPresent = 0.8
        , pOnsetNasal = 0.2
        , pOnsetLiquid = 0.3
        , pOnsetFricPresent = 0.5
        , wNumInter = [(0, 0.5), (1, 0.3), (2, 0.15), (3, 0.05)]
        , wPlace = [(Bilabial, 0.2), (Alveolar, 0.3), (Velar, 0.5)]
        , wLiquid = [(ClearL, 0.5), (DarkL, 0.3), (Rhotic, 0.2)]
        , pMidSlotPresent = 0.4
        , pCodaPresent = 0.7
        , pCodaNasal = 0.3
        , pCodaLiquidPresent = 0.4
        , pCodaLiquidDouble = 0.1
        , pCodaFricPresent = 0.2
        , pMidSlotArchNasal = 0.1
        , pVoiced = 0.6
        }
  let (result, log) = runMyMonadBS seed profile gen
  mapM_ putStrLn log
  print result
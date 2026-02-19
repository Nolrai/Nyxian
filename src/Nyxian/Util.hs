{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Utility functions for InheritenceOfLot simulation
module Nyxian.Util where
import Data.List qualified as List
import GHC.Stack (HasCallStack)
import Data.Set
import Data.Set qualified as Set
import Data.Maybe (catMaybes)
import Control.Exception (assert)
import System.Console.ANSI
import System.IO (hFlush, stdout)
import System.Random.SplitMix (SMGen, splitSMGen)
import System.Random.SplitMix qualified as R
import System.Random.Stateful qualified as R
import Data.Foldable as F
import Control.Monad.State.Strict
import Control.Parallel.Strategies (parMap, rpar)
import Data.Word (Word64)
import Data.Bits (testBit, Bits (shiftR))
import Control.Monad.RWS.Strict (MonadRWS)
import System.Random.Stateful (STGen)
import Data.Kind (Type)

class
  (MonadRWS (Env m) (Log m) BitSource m, MonadRWS (Env m) (Log m) SMGen (SM m)) =>
  MonadRandom' m where
    type Env m :: Type
    type Log m :: Type
    type SM m :: Type -> Type
    asBitSource :: (SM m) a -> m a
    runBitsource :: MonadRandom' m => m a -> SM m a
    
-- | Split a list into N chunks, distributing extra elements to the first chunks.
--   Each chunk will have either baseSize or baseSize+1 elements.
chunksOfN :: Int -> [a] -> [[a]]
chunksOfN n xs =
  let (baseSize, extra) = length xs `quotRem` n
      -- The first 'extra' chunks get one extra element
      sizes = replicate extra (baseSize+1) ++ replicate (n-extra) baseSize
  in go sizes xs
  where
    go [] _ = []
    go (s:ss) ys = let (h, t) = List.splitAt s ys in h : go ss t

-- | Run a function in parallel on ~n interleaved chunks of a list, using separate SMGen generators for each chunk.
-- The input list is first split into n chunks (with 'chunksOfN'), then transposed so each chunk contains every nth element.
-- This helps balance work across threads. Each chunk is processed in parallel with its own independent random generator.
runParOnChunks :: forall m a b. MonadRandom' m =>
  Int -> (a -> SM m b) -> [a] -> SM m [b]
runParOnChunks n f xs = do
  let chunks = chunksOfN n xs
      chunks' = List.transpose chunks
      f' = mapM f
  seedGen <- state splitSMGen
  pure . concat $ splitMap @m f' seedGen chunks'

-- | Generate n independent SMGen generators from a root generator.
splitSMGenList :: Int -> SMGen -> [SMGen]
splitSMGenList n gen = List.take n $ iterate (snd . splitSMGen) gen

-- | Map a function over a list in parallel, using a separate SMGen generator for each element.
-- The input list is processed in parallel with its own independent random generator.
-- This is a helper function used by 'runParOnChunks' to apply the function to each chunk in parallel.
splitMap ::
  forall m a b.
  MonadRandom' m =>
  (a -> SM m b) -> SMGen -> [a] -> [b]
splitMap f' seedGen xs =
  let gens = splitSMGenList (length xs) seedGen
      f'' gen x = evalState (runBitsource $ f' x) gen
  in parMap rpar (uncurry f'') (zip gens xs)

bernoulliWhen :: MonadRandom' m => Double -> m a -> m (Maybe a)
bernoulliWhen p f = do
  r <- bernoulli p
  if r then Just <$> f else pure Nothing

-- | Perform a Bernoulli trial with probability p.
--   Returns True with probability p, and False otherwise.
bernoulli :: MonadRandom' m => Double -> m Bool
bernoulli p =
  assert (p >= 0 && p <= 1) $ do
  (r :: Double) <- state (R.uniformR (0, 1))
  pure (r < p)

-- computes the chance of an event happening at least once out of five trials
probAtLeastOnceN :: MonadRandom' m => Int -> Double -> Double
probAtLeastOnceN n pr = 1 - (1 - pr) ^ (n :: MonadRandom' m => Int)

toMeanAndMedian :: MonadRandom' m => [Double] -> (Double, Double, Double)
toMeanAndMedian xs =
  case xs of
    [] -> (0/0, 0/0, 0/0)
    _ ->
      let total = F.sum xs
          n = F.length xs
          mean = total / fromIntegral n
          sorted = List.sort xs
          mid = n `div` 2
          median =
            if odd n
              then sorted !! mid
              else (sorted !! (mid - 1) + sorted !! mid) / 2
          variance = F.sum (fmap (\x -> (x - mean) ^ (2 :: Int)) xs) / fromIntegral n
          stdDev = sqrt variance
      in (mean, median, stdDev)

clearLineAndPrint :: String -> IO ()
clearLineAndPrint msg = do
  cursorUpLine 1         -- Move cursor up by one line
  clearLine              -- Clear the entire current line
  putStrLn msg             -- Print the new message
  hFlush stdout          -- Ensure the output is displayed immediately

randomEvenPartitionSet :: Ord a =>
  Set a -> m (Set a, Set a)
randomEvenPartitionSet s = do
  let xs = Set.toList s
      n = length xs `div` 2
  (left, right, _count) <- quickUnsort n xs
  pure (Set.fromList left, Set.fromList right)

-- | Randomly partition a list into two lists, with the number of elements in the first list being approximately n.

quickUnsort :: MonadRandom' m => Int -> [a] -> m ([a], [a], Int)
quickUnsort 0 xs = pure ([], xs, 0)
quickUnsort n xs = do
  (left, right, count) <- splitListRough xs
  case compare count n of
    EQ -> pure (left, right, count)
    LT -> do
      (newLeft, newRight, newCount) <- quickUnsort (n - count) right
      pure (left ++ newLeft, newRight, count + newCount)
    GT -> do
      (newLeft, newRight, newCount) <- quickUnsort n left
      pure (newLeft, newRight ++ right, newCount)

data BitSource = BitSource {
  bsGen :: SMGen,
  bsBuffer :: Word64,
  bsBitsLeft :: Word64
}

getBit :: MonadRandom' m => m Bool
getBit = do
  BitSource gen buffer bitsLeft <- get
  if bitsLeft == 0
    then do
      let (newBuffer, newGen) = R.nextWord64 gen
      put $ BitSource newGen newBuffer 64
      getBit
    else do
      let bit = testBit buffer 0
      put $ BitSource gen (shiftR buffer 1) (bitsLeft - 1)
      return bit

splitListRough :: [a] -> m ([a], [a], Int)
splitListRough s = go s [] [] 0
  where
    go [] acc1 acc2 n = pure (acc1, acc2, n)
    go (x:xs) acc1 acc2 n = do
      b <- getBit
      if b
        then go xs (x : acc1) acc2 (n + 1)
        else go xs acc1 (x : acc2) n

-- | Choose an element from a weighted list using SMGen.
weighted :: MonadRandom' m => [(a, Double)] -> m a
weighted xs0 = do
  let xs = List.filter (\(_,w) -> w > 0) xs0
  assert (not (F.null xs)) $
    let total = sum (snd <$> xs)
        cumulative = scanl1 (\(_,acc) (v,w) -> (v, acc + w)) xs
    in assert (total > 0) $ do
         r <- state (R.uniformR (0, total))
         pure $ go r cumulative
  where
    go !r ((v,acc):rest)
      | acc >= r  = v
      | otherwise = go r rest
    go _ [] = error "weighted: unreachable (empty after asserts)"
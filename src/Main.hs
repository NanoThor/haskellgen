{-# LANGUAGE NamedFieldPuns, TupleSections #-}

--
-- Copyright (c) 2006 Don Stewart - http://www.cse.unsw.edu.au/~dons/
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

import System.Environment
import Control.Applicative
import Control.Arrow (second)
import Control.Monad (liftM, replicateM)
import Control.Monad.Random
import Data.Function (on)
import Data.List (minimumBy, sortBy, nub, (\\))
import Data.Ord (comparing)
import Text.Printf (printf)


type Gene = String
target :: Gene
target = "Hello, world!"

mate :: RandomGen g => Gene -> Gene -> Rand g Gene
mate g1 g2 = (++) <$> flip take g1 <*> flip drop g2 <$> pivot
  where pivot = getRandomR (0, length g1 - 1)

mutate :: RandomGen g => Gene -> Rand g Gene
mutate g = (uncurry (++) .) . second . (:) <$> delta <*> parts
  where
    delta = getRandomR (' ', 'z')
    idx = getRandomR (0, length g - 1)
    parts = second tail . flip splitAt g <$> idx

fitness :: Gene -> Int
fitness = sum . map abs . zipWith ((-) `on` fromEnum) target

randomGene :: RandomGen g => Rand g Gene
randomGene = replicateM (length target) $ getRandomR (' ', 'z')

data PopInfo = PopInfo { size :: Int, crossover :: Float, elitism :: Float, mutation :: Float }

type Population = (PopInfo, [Gene])
defaultPop :: PopInfo
defaultPop = PopInfo 1024 0.8 0.1 0.03

randomPop :: RandomGen g => PopInfo -> Rand g Population
randomPop = liftA2 (,) <$> pure <*> flip replicateM randomGene . size

tournamentSize :: Int
tournamentSize = 3

tournamentSelection :: RandomGen g => Population -> Rand g Gene
tournamentSelection (info, genes) =
   minimumBy (comparing fitness) .  map (genes !!) <$>
   replicateM tournamentSize (getRandomR (0, size info - 1))

twoM :: Monad m => m a -> m (a, a)
twoM = liftM (\[x,y] -> (x,y)) . replicateM 2

selectParents :: RandomGen g => Population -> Rand g (Gene, Gene)
selectParents = twoM . tournamentSelection

evolve :: RandomGen g => Population -> Rand g Population
evolve p@(info@(PopInfo{size, crossover, elitism, mutation}), genes) =
  (info,) . sortBy (comparing fitness) . (take idx genes ++) <$> replicateM (size - idx) (twoM getRandom >>= go)
  where
    idx = round (fromIntegral size * elitism)
    go (r1,r2)
      | r1 <= crossover =  selectParents p >>= uncurry mate >>= addChild r2
      | otherwise = addMutation r2
    addChild r c
      | r <= mutation = mutate c
      | otherwise = return c
    addMutation r
      | r <= mutation = mutate . (genes !!) =<< getRandomR (idx, size - 1)
      | otherwise = (genes !!) <$> getRandomR (idx, size - 1)

iterateUntil :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntil stop f = go
  where
  go x
    | stop x = return x
    | otherwise = f x >>= go

maxGenerations :: Int
maxGenerations = 16384

main::IO()
main = evalRandIO (randomPop defaultPop >>= iterateUntil done step . (, 0)) >>= result
  where
    step (p,gen) = (,) <$> evolve p <*> pure (gen+1)
    done ((_, g:_), generation) = generation == maxGenerations || fitness g == 0
    result ((_, g:_), generation)
      | generation == maxGenerations = putStrLn "Maximum generations reached without success."
      | fitness g == 0 = printf "Reached target (%d): %s\n" generation g
      | otherwise = putStrLn "Evolution is hard. Let's go shopping."

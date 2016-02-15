import System.Random
import Data.List
import Control.Monad
import Control.Monad.Random
import Graph

-- =====================================================================
-- Graph Stuffs
-- =====================================================================



-- =====================================================================
-- Gene Stuffs
-- =====================================================================
type Gene = [Int]

randomGene :: [Int] -> IO Gene
randomGene [] = error "nã é possível realizar esta operação"
randomGene [a] = return [a]
randomGene v =
  do
    r <- randomIO :: IO Int
    let
      p = mod r (length v)
      (le,ld) = splitAt p v
    x <- randomGene (le ++ tail ld)
    return (head ld : x)

fitness :: Gene -> Graph -> Int
fitness gene graph = fitaux (startVertice graph) gene graph

fitaux :: Int -> Gene -> Graph -> Int
fitaux v [] graph = getEdgeValue v (startVertice graph) graph
fitaux v gene graph = getEdgeValue v (head gene) graph + fitaux (head gene) (tail gene) graph

-- =====================================================================
-- Population Stuffs
-- =====================================================================

data PopInfo = PopInfo { popSize :: Int, crossoverFactor :: Float, elitism :: Float, mutation :: Float } deriving(Show, Eq)
defaultPop :: PopInfo
defaultPop = PopInfo 1024 0.8 0.1 0.03

data Population = Population {info :: PopInfo, genes :: [IO Gene]}

randomPop :: PopInfo -> Graph -> Population
randomPop pinfo graph =
  let
    genePopulation = randomPopAux (popSize pinfo) (vcount graph)
  in
    Population pinfo genePopulation

-- q : Quantidade :: Int
-- m : Maximo :: Int
-- retorno : Lista de Genes Gerados :: [Gene]
randomPopAux :: Int -> Int -> [IO Gene]
randomPopAux 0 _ = []
randomPopAux m q = (randomGene [0..(q-1)]) : randomPopAux (m-1) q

-- =====================================================================
-- crossover functions
-- =====================================================================
getRandom' :: IO Float
getRandom' =
  do
    g <- randomIO :: IO Word
    return (fromIntegral g / fromIntegral (maxBound :: Word))

getPivot :: Int -> IO Int
getPivot l = do x <- getRandom'
                return $ round (x* fromIntegral l)

crossover :: Gene -> Gene -> IO (Gene, Gene)
crossover l1 l2 =
  do
    s <- getPivot (length l1)
    return (cross s l1 l2 , cross s l2 l1)

cross :: Int -> Gene -> Gene -> Gene
cross p str1 str2 = take p str1 ++ drop p str2

-- =====================================================================
-- MAIN
-- =====================================================================
main :: IO ()
main = return ()




-- main :: IO()
-- main =
--   do
--     lines <- crossover [10..19] [20..29]
--     print lines

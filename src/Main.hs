import System.Random
import System.IO
import Data.List
import Control.Monad
import Control.Monad.Random
import Graph
import Cross

import Data.List
import Data.Tuple
import Data.Function

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

-- fazer o fitness

percurseSize :: Gene -> Graph -> Int
percurseSize (gh:gt) graph = percurseSize' gh (gh:gt) graph

percurseSize' :: Int -> Gene -> Graph -> Int
percurseSize' _ [] _            = error "Gene não deve ser []"
percurseSize' a [n] graph       = getEdgeValue a n graph
percurseSize' a (n1:n2:r) graph = getEdgeValue n1 n2 graph + percurseSize' a (n2:r) graph

fitness :: Gene -> Population -> Graph -> Float
fitness g p graph =
   let
     r = somaTodosOsTamanhosDeRotaNaPopulacao p graph
     s = r - (percurseSize g graph)
     l = length $ genes p
   in fromIntegral s / fromIntegral (r * (l -1))

somaTodosOsTamanhosDeRotaNaPopulacao :: Population -> Graph-> Int
somaTodosOsTamanhosDeRotaNaPopulacao p graph = foldr (+) 0 $ map (flip percurseSize graph) (genes p)

roleta :: Population -> Graph -> IO Gene
roleta pop graph =
  do
    r <- getRandom'
--    print r
    let
      l = map (\g -> (g ,fitness g pop graph)) (genes pop)
--    print l
    return (roleta' l r)

roleta' :: [(Gene, Float)] -> Float -> Gene
roleta' [] _= error "Erro!"
roleta' ((g, c):as) r
  | r <= c = g
  | otherwise = roleta' (as) (r - c)



-- =====================================================================
-- Population Stuffs
-- =====================================================================

data PopInfo = PopInfo { popSize :: Int, crossoverFactor :: Float, elitism :: Float, mutation :: Float } deriving(Show, Eq)
defaultPop :: PopInfo
defaultPop = PopInfo 10 0.8 0.1 0.03

data Population = Population {info :: PopInfo, genes :: [Gene]} deriving (Show, Eq)

-- ==============================
randomPop :: PopInfo -> Graph -> IO Population
randomPop pinfo g =
  do
    genePopulation <- randomPopAux (popSize pinfo) (vcount g)
    return (Population pinfo genePopulation)

-- q : Quantidade :: Int
-- m : Maximo :: Int
-- retorno : Lista de Genes Gerados :: [Gene]
randomPopAux :: Int -> Int -> IO [Gene]
randomPopAux 0 _ = return []
randomPopAux m q =
  do
    subList <- randomPopAux (m-1) q
    h <- randomGene [0..q-1]
    return (h : subList)
-- ==============================



-- TODO: Ainda não testei essa funções. Seria interessante ver se tá certo. Já teria meio caminho andado.
-- -- ==============================
-- naturalSelection :: Population -> Population
-- naturalSelection pop =
{-
natSelFitList :: [Gene] -> Graph -> [(Gene,Int)]
natSelFitList [] _ = []
natSelFitList (x:xs) g = (x, fitness x g) : natSelFitList xs g
-}
-- TODO: aqui seria pra retornar genes mais fortes. Ainda não tá dropando metade da lista. Fazer isso.
killWeakGenes :: [(Gene,Int)] -> [Gene]
killWeakGenes [] = []
killWeakGenes list =
  let
    sortedList = sortBy (compare `on` snd) list
    killedList = map (\(x,_) -> x) sortedList
  in killedList

-- -- ==============================

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

-- crossover :: Gene -> Gene -> IO (Gene, Gene)
-- crossover l1 l2 =
--   do
--     s <- getPivot (length l1)
--     return (cross s l1 l2 , cross s l2 l1)

cross :: Int -> Gene -> Gene -> Gene
cross p str1 str2 = take p str1 ++ drop p str2

-- =====================================================================
-- MAIN
-- =====================================================================

-- TODO: INICIO DO LAÇO
main :: IO ()
main = do
  graph <- loadGraphFromFile "graph.txt"
  startPop <- randomPop defaultPop graph
  let it = 1024
  finalPop <- loopGenerationUtil startPop graph it
  let geneRes = nub (genes finalPop)
  let gene = head geneRes
  appendFile "MenoresCaminhos.txt" ("Populacao: "++ (show. popSize . info) finalPop ++ " | " ++show gene ++ " Geracoes: " ++ show it ++ "\n")
  print $ geneRes


-- TODO: GERA NOVA POPULAÇÃO DE GENES APLICANDO A ROLETA
geraNovaPop :: Population -> Graph -> Int -> IO [Gene]
geraNovaPop _ _ 0 = return []
geraNovaPop p g s =
  do
    gene <- roleta p g
    gene' <- geraNovaPop p g (s-1)
    return (gene : gene')

-- TODO: TENTA SIMULAR UM LOOP...TA TENDO UNS CONFLITOS, MAS ACHO QUE DÁ PRA SACAR A IDEIA....
loopGenerationUtil :: Population -> Graph -> Int -> IO Population
loopGenerationUtil p _ 0 = return p
loopGenerationUtil p g m =
  do
    let geneSampleLenght = length(head(genes p)) - 1
    r0 <- randomIO
    let rand0 = mod r0 geneSampleLenght
    r1 <- randomIO
    let rand1 = mod r1 geneSampleLenght
    let mutedGenes = mutaGenes (genes p) rand0 rand1
    let mutedPop = Population defaultPop mutedGenes
    newGenes <- geraNovaPop mutedPop g (popSize defaultPop)
    let newpop = (Population defaultPop newGenes)
    nextGen <- loopGenerationUtil newpop g (m-1)
    return nextGen

-- TODO: MUTA TODOS OS GENES COM O NUMEROS RANDOMS GERADOS.
mutaGenes :: [Gene] -> Int -> Int -> [Gene]
mutaGenes [] _ _ = []
mutaGenes (gh:gt) f0 f1 = muta gh f0 f1 : mutaGenes gt f0 f1

-- main :: IO()
-- main =
--   do
--     lines <- crossover [10..19] [20..29]
--     print lines

module Graph where
import System.IO
import Data.Char

data Graph = Graph { vcount :: Int,  edges :: [[Int]] } deriving (Show, Eq)

getEdgeValue :: Int -> Int -> Graph -> Int
getEdgeValue x y g = (edges g !! x ) !! y

loadGraphFromFile :: FilePath -> IO Graph
loadGraphFromFile path =
  do
    hFile <- openFile path ReadMode
    vertCount <- hGetLine hFile
    let vertCount' = read vertCount :: Int
    edges <- parseFileEdgeLine hFile vertCount'
    hClose hFile
    return (Graph vertCount' edges)


parseFileEdgeLine :: Handle -> Int -> IO [[Int]]
parseFileEdgeLine _ 0 = return []
parseFileEdgeLine hFile i = do
  line <- hGetLine hFile
  r <- parseFileEdgeLine hFile (i-1)
  return $ parseLine line : r

parseLine :: String -> [Int]
parseLine = map read . words

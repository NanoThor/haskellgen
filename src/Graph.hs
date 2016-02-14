module Graph where
import System.IO
import Data.Char

data Graph = Graph { vcount :: Int,  edges :: [[Int]], startVertice :: Int } deriving (Show, Eq)

getEdgeValue :: Int -> Int -> Graph -> Int
getEdgeValue x y g = (edges g !! x ) !! y

loadGraphFromFile :: FilePath -> IO Graph
loadGraphFromFile path =
  do
    hFile <- openFile path ReadMode
    infoLine <- hGetLine hFile
    let infoLine' = parseLine infoLine
    let vertCount' = head infoLine'
    let startVertice = infoLine'!!1
    edges <- parseFileEdgeLine hFile vertCount'
    hClose hFile
    return (Graph vertCount' edges startVertice)


parseFileEdgeLine :: Handle -> Int -> IO [[Int]]
parseFileEdgeLine _ 0 = return []
parseFileEdgeLine hFile i = do
  line <- hGetLine hFile
  r <- parseFileEdgeLine hFile (i-1)
  return $ parseLine line : r

parseLine :: String -> [Int]
parseLine = map read . words

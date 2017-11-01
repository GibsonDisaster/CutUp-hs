module Main where
  import System.Random
  import System.Environment
  import Data.Array.IO
  import Control.Monad

  data Line = Line Int [String] deriving (Show, Eq)
  type Song = [Line]

  shuffle :: [a] -> IO [a]
  shuffle xs = do
          ar <- newArray n xs
          forM [1..n] $ \i -> do
              j <- randomRIO (i,n)
              vi <- readArray ar i
              vj <- readArray ar j
              writeArray ar j vi
              return vj
    where
      n = length xs
      newArray :: Int -> [a] -> IO (IOArray Int a)
      newArray n xs =  newListArray (1,n) xs

  main :: IO ()
  main = do
    a <- getArgs
    ws <- readFile (head a)
    let al = getAllWords ws
    let cl = countLines ws
    shuffled <- shuffle al
    writeFile (last a) $ unlines (mixUpSong cl shuffled)

  getAllWords :: String -> [String]
  getAllWords str = concat $ map (\l -> words l) (lines str)

  countLines :: String -> Song
  countLines song = map (\l -> (Line (length (words l)) (words l))) (lines song)

  lengthOf :: Line -> Int
  lengthOf (Line l _) = l

  contentOf :: Line -> [String]
  contentOf (Line _ s) = s

  mixUpSong :: Song -> [String] -> [String]
  mixUpSong _ [] = []
  mixUpSong [] _ = []
  mixUpSong (x:xs) ws = ([unwords (take (lengthOf x) ws)] ++ [""]) ++ (mixUpSong xs (drop (lengthOf x) ws))
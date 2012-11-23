-- | Command line utility which randomly generates a list of points.
-- Accepts the number of points as a parameter of the command line.

import System.Environment (getArgs)
import System.Random (getStdGen, randomRs)

main = do
    [n] <- getArgs
    gen <- getStdGen
    putStrLn n
    putStrLn $ unlines $ take (read n) $ points $ randomRs (0, 1) gen
  where
    points :: [Double] -> [String]
    points (x:y:xs) = (show x ++ " " ++ show y) : points xs
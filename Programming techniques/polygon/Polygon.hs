import Data.List (sortBy)
import Data.Function (on)

type Point = (Double, Double)

main :: IO ()
main = do
    _ <- getLine
    ls <- getContents
    let ps = polygon $ points ls
    print (length ps + 1)
    putStr $ unlines $ map showPoint $ ps
    putStrLn $ showPoint $ head ps
  where
    showPoint (x, y) = show x ++ " " ++ show y

points :: String -> [Point]
points = go . words
  where
    go []       = []
    go (x:y:xs) = (read x, read y) : go xs

polygon :: [Point] -> [Point]
polygon ps =
    first : sortBy cmpSlope ps'
  where
    (first@(x, y):ps') = sortBy (compare `on` fst) ps
    cmpSlope (x1, y1) (x2, y2) =
        let (dx1, dy1) = (x1 - x, y1 - y)
            (dx2, dy2) = (x2 - x, y2 - y)
        in compare (dx1 * dy2) (dx2 * dy1)
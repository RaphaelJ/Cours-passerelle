{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Criterion.Main
import Data.Array.Repa.Eval
import Data.Array.Repa hiding (size, (++), map)
import Data.List
import Data.Word
import System.Random

size :: Int
size = 1000

type Board r = Array r DIM2 Word8

main :: IO ()
-- main = print $ nextGen board ! (Z :. 0 :. 1)
main = defaultMain [
      bench "Single thread" $ whnf (computeUnboxedS . nextGen) board
    , bench "Parallel" $ whnfIO $ computeUnboxedP $ nextGen $ board
    ]
  where
    bounds = Z :. size :. size

    board :: Board U
    !board = fromListUnboxed bounds $ take (size * size) $
        map (\v -> (if v then 1 else 0)) $ randoms (mkStdGen 1)
    !prev = fromListUnboxed (Z :. size) (size-1 : [0..(size-2)])
    !next = fromListUnboxed (Z :. size) ([1..(size-1)] ++ [0])

    nextGen :: Board U -> Board D 
    nextGen board = fromFunction bounds $ \(Z :. y :. x) ->
        let x1 = prev `unsafeIndex` (Z :. x)
            x2 = next `unsafeIndex` (Z :. x)
            y1 = prev `unsafeIndex` (Z :. y)
            y2 = next `unsafeIndex` (Z :. y)
            cell = board `unsafeIndex` (Z :. y  :. x )
            a    = board `unsafeIndex` (Z :. y1 :. x1)
            b    = board `unsafeIndex` (Z :. y1 :. x )
            c    = board `unsafeIndex` (Z :. y1 :. x2)
            d    = board `unsafeIndex` (Z :. y  :. x1)
            e    = board `unsafeIndex` (Z :. y  :. x2)
            f    = board `unsafeIndex` (Z :. y2 :. x1)
            g    = board `unsafeIndex` (Z :. y2 :. x )
            h    = board `unsafeIndex` (Z :. y2 :. x2)
            n = a + b + c + d + e + f + g + h
        in if n == 3 || (n == 2 && cell == 1) then 1 else 0
    {-# INLINE nextGen #-}

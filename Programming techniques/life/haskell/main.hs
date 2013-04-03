{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Criterion.Main
import Data.Array.Repa.Eval
import Data.Array.Repa hiding (size, (++), map)
import Data.List
import Data.Word
import System.Random

w, h :: Int
(w, h) = (1024, 1024)

type Board r = Array r DIM2 Word8

main :: IO ()
main = defaultMain [
      bench "Single thread" $ whnf (computeUnboxedS . step) board
    , bench "Parallel" $ whnfIO $ computeUnboxedP $ step $ board
    ]
  where
    size = Z :. h :. w
    w1 = w - 1
    h1 = h - 1

    board :: Board U
    !board = fromListUnboxed size $ take (w * h) $
        map (\v -> if v then 1 else 0) $ randoms (mkStdGen 1)
    !x1s = fromListUnboxed (Z :. w) (w-1 : [0..(w-2)])
    !x2s = fromListUnboxed (Z :. w) ([1..(w-1)] ++ [0])
    !y1s = fromListUnboxed (Z :. h) (h-1 : [0..(h-2)])
    !y2s = fromListUnboxed (Z :. h) ([1..(h-1)] ++ [0])

    step board = fromFunction size $ \(Z :. y :. x) ->
        let x1 = x1s `unsafeIndex` (Z :. x)
            x2 = x2s `unsafeIndex` (Z :. x)
            y1 = y1s `unsafeIndex` (Z :. y)
            y2 = y2s `unsafeIndex` (Z :. y)
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
        in n == 3 || (n == 2 && cell == 1)
    {-# INLINE step #-}

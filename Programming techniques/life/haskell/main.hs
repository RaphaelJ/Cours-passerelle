{-# LANGUAGE BangPatterns #-}
import Criterion.Main
import Data.Array.Repa hiding (size, (++), map)
import Data.Word
import qualified Data.Vector.Unboxed as V
import System.Random (mkStdGen, randomR)

size :: Int
size = 1000

type Board r = Array r DIM2 Word8

main :: IO ()
main = defaultMain [
      bench "Single thread" $ whnf (computeUnboxedS . nextGen) board
    , bench "Parallel" $ whnfIO $ computeUnboxedP $ nextGen $ board
    ]
  where
    bounds = Z :. size :. size

    board :: Board U
    !board = fromUnboxed bounds $ V.unfoldrN (size * size)
                                             (Just . randomR (0, 1))
                                             (mkStdGen 1)

    !prev = fromListUnboxed (Z :. size) (size-1 : [0..(size-2)])
    !next = fromListUnboxed (Z :. size) ([1..(size-1)] ++ [0])

--     !prev = fromListUnboxed (Z :. size) $ map (* size) (size-1 : [0..(size-2)])
--     !next = fromListUnboxed (Z :. size) $ map (* size) ([1..(size-1)] ++ [0])

    nextGen :: Board U -> Board D
    nextGen board = fromFunction bounds $ \(Z :. y :. x) ->
        let x1 = prev `index` ix1 x
            x2 = next `index` ix1 x
            y1 = prev `index` ix1 y
            y2 = next `index` ix1 y
            cell = board `index` ix2 y  x
            a    = board `index` ix2 y1 x1
            b    = board `index` ix2 y1 x
            c    = board `index` ix2 y1 x2
            d    = board `index` ix2 y  x1
            e    = board `index` ix2 y  x2
            f    = board `index` ix2 y2 x1
            g    = board `index` ix2 y2 x
            h    = board `index` ix2 y2 x2
--         let x1 = prev `unsafeIndex` ix1 x
--             x2 = next `unsafeIndex` ix1 x
--             y1 = prev `unsafeIndex` ix1 y
--             y2 = next `unsafeIndex` ix1 y
--             dy1 = y1
--             dy  = y * size
--             dy2 = y2
--             cell = board `linearIndex` (dy  + x )
--             a    = board `linearIndex` (dy1 + x1)
--             b    = board `linearIndex` (dy1 + x )
--             c    = board `linearIndex` (dy1 + x2)
--             d    = board `linearIndex` (dy  + x1)
--             e    = board `linearIndex` (dy  + x2)
--             f    = board `linearIndex` (dy2 + x1)
--             g    = board `linearIndex` (dy2 + x )
--             h    = board `linearIndex` (dy2 + x2)
            n = a + b + c + d + e + f + g + h
        in if n == 3 || (n == 2 && cell == 1) then 1 else 0
    {-# INLINE nextGen #-}

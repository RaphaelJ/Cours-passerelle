{-# LANGUAGE BangPatterns #-}
import Criterion.Main
import Data.Array.Repa hiding (size, (++), map)
import Data.Word
import qualified Data.Vector.Unboxed as V
import System.Random (mkStdGen, randomR)

type Board r = Array r DIM2 Word8

main :: IO ()
main = defaultMain [
      bench "144"   $ whnfIO $ computeUnboxedP $ nextGen board144
    , bench "576"   $ whnfIO $ computeUnboxedP $ nextGen board576
    , bench "1008"  $ whnfIO $ computeUnboxedP $ nextGen board1008
    , bench "2592"  $ whnfIO $ computeUnboxedP $ nextGen board2592
    , bench "5040"  $ whnfIO $ computeUnboxedP $ nextGen board5040
    , bench "10080" $ whnfIO $ computeUnboxedP $ nextGen board10080
    ]
  where
    !board144   = randBoard 144
    !board576   = randBoard 576
    !board1008  = randBoard 1008
    !board2592  = randBoard 2592
    !board5040  = randBoard 5040
    !board10080 = randBoard 10080

    -- Initialises a board randomly.
    randBoard :: Int -> Board U
    randBoard size =
        let bounds = Z :. size :. size
        in fromUnboxed bounds $ V.unfoldrN (size * size) (Just . randomR (0, 1))
                                           (mkStdGen 1)

    -- Computes the next state of the board.
    nextGen :: Board U -> Board D
    nextGen !board =
        let !bounds@(Z :. size :. _) = extent board
            !prev = fromListUnboxed (Z :. size) (size-1 : [0..(size-2)])
            !next = fromListUnboxed (Z :. size) ([1..(size-1)] ++ [0])
        in fromFunction bounds $ \(Z :. y :. x) ->
            let x1 = prev `unsafeIndex` ix1 x
                x2 = next `unsafeIndex` ix1 x
                y1 = prev `unsafeIndex` ix1 y
                y2 = next `unsafeIndex` ix1 y
                cell = board `unsafeIndex` ix2 y  x
                a    = board `unsafeIndex` ix2 y1 x1
                b    = board `unsafeIndex` ix2 y1 x
                c    = board `unsafeIndex` ix2 y1 x2
                d    = board `unsafeIndex` ix2 y  x1
                e    = board `unsafeIndex` ix2 y  x2
                f    = board `unsafeIndex` ix2 y2 x1
                g    = board `unsafeIndex` ix2 y2 x
                h    = board `unsafeIndex` ix2 y2 x2
                n = a + b + c + d + e + f + g + h
            in if n == 3 || (n == 2 && cell == 1) then 1 else 0
    {-# INLINE nextGen #-}

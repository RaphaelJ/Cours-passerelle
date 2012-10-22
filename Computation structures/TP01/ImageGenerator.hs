-- | Generates an bit-encoded black and white image for the beta assembler.
-- Raphael Javaux - October 2012.
-- 
-- Compiles with the GHC Haskell compiler :
-- > ghc ImageGenerator.hs
-- Requires the repa-devil library :
-- > cabal install repa-devil
-- Usage :
-- ./ImageGenerator <image path>
-- Runs with grey scale, RGB and RGBA images.

import Control.Monad
import Data.List
import Data.Word
import Data.Bits
import System.Environment (getArgs)

import Data.Array.Repa (Z (..), (:.) (..), extent, (!))
import Data.Array.Repa.IO.DevIL

screenWidth = 256

main = do
    args <- getArgs
    case args of
        [path] -> do
            img <- runIL $ readImage path
            let (w, h) = imgSize img
            if w > screenWidth
                then putStrLn $ "The image is larger than " ++ show screenWidth
                else do
                    let paddingLen = (screenWidth - w) `quot` 32
                    let padding = "STORAGE(" ++ show paddingLen ++ ")"

                    forM_ (bitmapWords $ toBitmap img) $ \line -> do
                        forM_ line $ \word ->
                            putStr $ "LONG(" ++ show word ++ ") "
                        putStrLn padding

                    putStrLn $ "w: LONG(" ++ show w ++ ")"
                    putStrLn $ "h: LONG(" ++ show h ++ ")"
        _ ->
            putStrLn "Usage : ./ImageGenerator <image path>"
  where
    imgSize (RGBA arr) = let (Z :. h :. w :. _) = extent arr in (w, h)
    imgSize (RGB arr)  = let (Z :. h :. w :. _) = extent arr in (w, h)
    imgSize (Grey arr) = let (Z :. h :. w) = extent arr in (w, h)
    imgSize _ = error "Unsupported image encoding"

-- | Returns the image as a stream of bits.
toBitmap :: Image -> [[Bool]]
toBitmap img =
    [ [ boolPoint x y | x <- [0..(w-1)] ] | y <- [(h-1),(h-2)..0] ]
  where
    (Z :. h :. w) =
        case img of
             RGBA arr -> toDIM2 $ extent arr
             RGB arr -> toDIM2 $ extent arr
             Grey arr -> extent arr
    toDIM2 (Z :. h :. w :. _) = Z :. h :. w

    -- Converts the pixel to a binary value.
    boolPoint x y =
        case img of
             RGBA arr -> pointToBit $ arr `fromDIM3Pt` (Z :. y :. x)
             RGB arr  -> pointToBit $ arr `fromDIM3Pt` (Z :. y :. x)
             Grey arr -> pointToBit $ arr ! (Z :. y :. x)
    pointToBit val = val <= 127
    arr `fromDIM3Pt` pt@(Z :. y :. x) =
        let r = (arr ! (pt :. 0)) `quot` 3
            g = (arr ! (pt :. 1)) `quot` 3
            b = (arr ! (pt :. 2)) `quot` 3
        in (r + g + b)

-- | Encode the binary image in 32bits words.
bitmapWords :: [[Bool]] -> [[Word32]]
bitmapWords img =
    map (map toWord . group32) img
  where
    group32 [] = []
    group32 xs =
        let (gs, ns) = splitAt 32 xs
        in gs : group32 ns

    toWord bs = foldl' step 0 (zip bs [0..31])
    step acc (True, n)  = acc `setBit` n
    step acc (False, _) = acc
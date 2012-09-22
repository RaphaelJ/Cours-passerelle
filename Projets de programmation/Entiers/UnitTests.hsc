{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign (Ptr)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Modifiers (NonZero (..))

import Debug.Trace

#include "UnitTestsWrapper.h"

type Signe = CInt
data CEntier = CEntier Signe (Ptr ())
type Entier = Ptr CEntier

main = defaultMain [
      testProperty "Comparer" $ propComparer
    , testProperty "Addition" $ propEntier (+) addition
    , testProperty "Soustraction" $ propEntier (-) soustraction
    , testProperty "Multiplication" $ propEntier (*) multiplication
    , testProperty "Division" propDivision
    ]

foreign import ccall unsafe "UnitTestsWrapper.h d_lireEntier"
     c_lireEntier :: CString -> IO Entier

lireEntier :: Integer -> Entier
lireEntier i = unsafePerformIO $
   newCString (show i) >>= c_lireEntier

foreign import ccall unsafe "UnitTestsWrapper.h d_comparer"
     c_comparer :: Entier -> Entier -> IO CInt

comparer :: Entier -> Entier -> Ordering
comparer a b = unsafePerformIO $ do
   res <- c_comparer a b
   return $ case res of
      a | a < 0  -> LT
        | a == 0 -> EQ
        | a > 0  -> GT

egal :: Entier -> Entier -> Bool
egal a b = comparer a b == EQ

foreign import ccall unsafe "UnitTestsWrapper.h d_addition"
     c_addition :: Entier -> Entier -> IO Entier
foreign import ccall unsafe "UnitTestsWrapper.h d_soustraction"
     c_soustraction :: Entier -> Entier -> IO Entier
foreign import ccall unsafe "UnitTestsWrapper.h d_multiplication"
     c_multiplication :: Entier -> Entier -> IO Entier
foreign import ccall unsafe "UnitTestsWrapper.h d_division"
     c_division :: Entier -> Entier -> IO Entier

addition, soustraction, multiplication, division :: Entier -> Entier -> Entier
addition a b = unsafePerformIO $ c_addition a b
soustraction a b = unsafePerformIO $ c_soustraction a b
multiplication a b = unsafePerformIO $ c_multiplication a b
division a b = unsafePerformIO $ c_division a b

foreign import ccall unsafe "UnitTestsWrapper.h d_afficherEntier"
     c_afficherEntier :: Entier -> IO ()

-- | Verifie si la comparaison d'Haskell est equivalente de la comparaison des
-- entiers.
propComparer :: Integer -> Integer -> Bool
propComparer a b = 
   let (a', b') = (lireEntier a, lireEntier b)
   in compare a b == comparer a' b'

-- | Verifie si la fonction sur les Integers donne le meme resultat que la 
-- fonction sur les Entiers.
propEntier :: (Integer -> Integer -> Integer)
           -> (Entier -> Entier -> Entier)
           -> Integer -> Integer
           -> Bool
propEntier fInt fEnt a b =
   let (a', b') = (lireEntier a, lireEntier b)
       (c, c') = (lireEntier $ fInt a b, fEnt a' b')
   in egal c c'

-- | Verifie la fonction division avec des nombres non nuls.
propDivision a (NonZero b) = propEntier quot division a b
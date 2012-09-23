{-# LANGUAGE ForeignFunctionInterface #-}

-- Projet de programmation: Calculs sur des entiers en precision arbitraire.
-- Raphael Javaux - Septembre 2012.
--
-- Utilise le framework de test unitaires QuickCheck de Haskell pour comparer
-- sur des valeurs aleatoires les resultats des fonctions de calcul des entiers
-- big-num de Haskell et ceux des fonctions du projets.

import Foreign (Ptr)
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)

import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Modifiers (NonZero (..))

#include "unit_tests_wrapper.h"

data CProjInteger
type ProjInteger = Ptr CProjInteger

type HaskellInt = Integer

main :: IO ()
main = defaultMain [
      testProperty "Compare" $ propCompare
    , testProperty "Addition" $ propInteger (+) addition
    , testProperty "Subtraction" $ propInteger (-) subtraction
    , testProperty "Multiplication" $ propInteger (*) multiplication
    , testProperty "Division" propDivision
    ]

foreign import ccall unsafe "unit_tests_wrapper.h d_readInteger"
     c_readInteger :: CString -> IO ProjInteger

readInteger :: HaskellInt -> ProjInteger
readInteger i = unsafePerformIO $
   newCString (show i) >>= c_readInteger

foreign import ccall unsafe "unit_tests_wrapper.h d_compare"
     c_compare :: ProjInteger -> ProjInteger -> IO CInt

compareProj :: ProjInteger -> ProjInteger -> Ordering
compareProj a b = unsafePerformIO $ do
   res <- c_compare a b
   return $ case res of
      _ | res < 0   -> LT
        | res == 0  -> EQ
        | otherwise -> GT

equals :: ProjInteger -> ProjInteger -> Bool
equals a b = compareProj a b == EQ

foreign import ccall unsafe "unit_tests_wrapper.h d_addition"
     c_addition :: ProjInteger -> ProjInteger -> IO ProjInteger
foreign import ccall unsafe "unit_tests_wrapper.h d_subtraction"
     c_subtraction :: ProjInteger -> ProjInteger -> IO ProjInteger
foreign import ccall unsafe "unit_tests_wrapper.h d_multiplication"
     c_multiplication :: ProjInteger -> ProjInteger -> IO ProjInteger
foreign import ccall unsafe "unit_tests_wrapper.h d_division"
     c_division :: ProjInteger -> ProjInteger -> IO ProjInteger

addition, subtraction, multiplication, division ::
   ProjInteger -> ProjInteger -> ProjInteger
addition a b = unsafePerformIO $ c_addition a b
subtraction a b = unsafePerformIO $ c_subtraction a b
multiplication a b = unsafePerformIO $ c_multiplication a b
division a b = unsafePerformIO $ c_division a b

-- foreign import ccall unsafe "unit_tests_wrapper.h d_afficherProjInteger"
--      c_afficherProjInteger :: ProjInteger -> IO ()

-- | Verifie si la comparaison d'Haskell est equivalente de la comparaison des
-- entiers.
propCompare :: HaskellInt -> HaskellInt -> Bool
propCompare a b = 
   let (a', b') = (readInteger a, readInteger b)
   in compare a b == compareProj a' b'

-- | Verifie si la fonction sur les Integers donne le meme resultat que la 
-- fonction sur les ProjIntegers.
propInteger :: (HaskellInt -> HaskellInt -> HaskellInt)
           -> (ProjInteger -> ProjInteger -> ProjInteger)
           -> HaskellInt -> HaskellInt
           -> Bool
propInteger fInt fProj a b =
   let (a', b') = (readInteger a, readInteger b)
       (c, c') = (readInteger $ fInt a b, fProj a' b')
   in equals c c'

-- | Verifie la fonction division avec des nombres non nuls.
propDivision :: HaskellInt -> NonZero HaskellInt -> Bool
propDivision a (NonZero b) = propInteger quot division a b
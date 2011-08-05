module Codec.Binary.Proquint.QuickChecks
    ( prop_proquints_are_roundtrippable
    , prop_magic_proquints_are_roundtrippable
    )
  where

import Data.Word
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Codec.Binary.Proquint

prop_proquints_are_roundtrippable :: (Eq a, ToProquint a) => a -> Property
prop_proquints_are_roundtrippable x = property $ fromProquint (toProquint x) == x

prop_magic_proquints_are_roundtrippable :: (Eq a, ToProquint a) => a -> Property
prop_magic_proquints_are_roundtrippable x = property $ fromProquint (toProquintWithMagic x) == x

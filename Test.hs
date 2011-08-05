module Main where

import Data.Word
import Test.QuickCheck.Property ( Property )
import Test.Framework ( defaultMain, testGroup )
import Test.Framework.Providers.QuickCheck2 ( testProperty )

import Codec.Binary.Proquint.QuickChecks

prop_word16 :: Word16 -> Property
prop_word16 = prop_proquints_are_roundtrippable

prop_word32 :: Word32 -> Property
prop_word32 = prop_proquints_are_roundtrippable

prop_word64 :: Word64 -> Property
prop_word64 = prop_proquints_are_roundtrippable

mprop_word16 :: Word16 -> Property
mprop_word16 = prop_magic_proquints_are_roundtrippable

mprop_word32 :: Word32 -> Property
mprop_word32 = prop_magic_proquints_are_roundtrippable

mprop_word64 :: Word64 -> Property
mprop_word64 = prop_magic_proquints_are_roundtrippable

tests = [ testGroup "without magic" [ testProperty "word16" prop_word16
                                    , testProperty "word32" prop_word32
                                    , testProperty "word64" prop_word64
                                    ]

        , testGroup "with magic" [ testProperty "word16" mprop_word16
                                 , testProperty "word32" mprop_word32
                                 , testProperty "word64" mprop_word64
                                 ]
        ]

main = defaultMain tests

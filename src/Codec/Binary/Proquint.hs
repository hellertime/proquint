module Codec.Binary.Proquint
    (
      -- ** Data Type
      Proquint
    , ToProquint (..)

      -- ** Encoding Proquints
    , encode16
    , encode32
    , encode64

      -- ** Decoding Proquints
    , decode16
    , decode32
    , decode64
    )
  where

import Data.Bits
import qualified Data.ByteString.Char8 as B8
import Data.Word

data Proquint = P B8.ByteString
              | MP B8.ByteString

instance Show Proquint where
    show (P bs)  = show bs
    show (MP bs) = show bs

toByteString :: Proquint -> B8.ByteString
toByteString (P bs)  = bs
toByteString (MP bs) = bs

class ToProquint a where
    toProquint          :: a -> Proquint
    fromProquint        :: Proquint -> a

    toProquintWithMagic :: a -> Proquint
    toProquintWithMagic = addMagic . toProquint

instance ToProquint Word16 where
    toProquint = P . encode16
    fromProquint = decode16 . toByteString . dropMagic

instance ToProquint Word32 where
    toProquint = P . encode32
    fromProquint = decode32 . toByteString . dropMagic

instance ToProquint Word64 where
    toProquint = P . encode64
    fromProquint = decode64 . toByteString . dropMagic

proquintMagic :: B8.ByteString
proquintMagic = B8.pack "0q-"

dropMagic :: Proquint -> Proquint
dropMagic (MP p)  = P $ B8.drop 3 p
dropMagic p       = p

addMagic :: Proquint -> Proquint
addMagic (P p)  = MP $ B8.append proquintMagic p
addMagic mp     = mp

consonantTable :: B8.ByteString
consonantTable = B8.pack "bdfghjklmnprstvz"

vowelTable :: B8.ByteString
vowelTable = B8.pack "aiou"

encode16 :: Word16 -> B8.ByteString
encode16 word = B8.pack $
    con 0xf000 12 :
    vow 0x0c00 10 :
    con 0x03c0 6  :
    vow 0x0030 4  :
    con 0x000f 0  : []
  where
    lookup :: B8.ByteString -> Word16 -> Int -> Char
    lookup tab mask shift = B8.index tab $ fromIntegral $ (word .&. mask) `shiftR` shift
    vow = lookup vowelTable
    con = lookup consonantTable

encode32 :: Word32 -> B8.ByteString
encode32 word = B8.append high low
  where
    high = encode16 $ fromIntegral (word `shiftR` 16) 
    low  = encode16 $ fromIntegral (word .&. 0xffff)

encode64 :: Word64 -> B8.ByteString
encode64 word = B8.append high low
  where
    high = encode32 $ fromIntegral (word `shiftR` 32)
    low  = encode32 $ fromIntegral (word .&. 0xffffffff)

decode16 :: B8.ByteString -> Word16
decode16 proquint = B8.foldl' decode 0 $ B8.take 5 proquint
  where
    -- consonants
    decode word 'b' = word `shiftL` 4 + 0
    decode word 'd' = word `shiftL` 4 + 1
    decode word 'f' = word `shiftL` 4 + 2
    decode word 'g' = word `shiftL` 4 + 3
    decode word 'h' = word `shiftL` 4 + 4
    decode word 'j' = word `shiftL` 4 + 5
    decode word 'k' = word `shiftL` 4 + 6
    decode word 'l' = word `shiftL` 4 + 7
    decode word 'm' = word `shiftL` 4 + 8
    decode word 'n' = word `shiftL` 4 + 9
    decode word 'p' = word `shiftL` 4 + 10
    decode word 'r' = word `shiftL` 4 + 11
    decode word 's' = word `shiftL` 4 + 12
    decode word 't' = word `shiftL` 4 + 13
    decode word 'v' = word `shiftL` 4 + 14
    decode word 'z' = word `shiftL` 4 + 15
    -- vowels
    decode word 'a' = word `shiftL` 2 + 0
    decode word 'i' = word `shiftL` 2 + 1
    decode word 'o' = word `shiftL` 2 + 2
    decode word 'u' = word `shiftL` 2 + 3

decode32 :: B8.ByteString -> Word32
decode32 proquint = high .|. low
  where
    high :: Word32
    high = (fromIntegral $ decode16 proquint) `shiftL` 16 
    low :: Word32
    low = fromIntegral $ decode16 $ B8.drop 5 proquint

decode64 :: B8.ByteString -> Word64
decode64 proquint = high .|. low
  where
    high :: Word64
    high = (fromIntegral $ decode32 proquint) `shiftL` 32
    low :: Word64
    low = fromIntegral $ decode32 $ B8.drop 10 proquint

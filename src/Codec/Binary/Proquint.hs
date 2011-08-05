{-# LANGUAGE OverloadedStrings #-}
module Codec.Binary.Proquint
    (
    )
  where

import Data.Bits
import qualified Data.ByteString.Char8 as B8
import Data.List ( genericIndex )
import Data.Word

consonantTable :: B8.ByteString
consonantTable = "bdfghjklmnprstvz"

vowelTable :: B8.ByteString
vowelTable = "aiou"

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

encodeWithSep32 :: Char -> Word32 -> B8.ByteString
encodeWithSep32 sep word = B8.append high $ B8.cons sep low
  where
    high = encode16 $ fromIntegral (word `shiftR` 16) 
    low  = encode16 $ fromIntegral (word .&. 0xffff)

encode64 :: Word64 -> B8.ByteString
encode64 word = B8.append high low
  where
    high = encode32 $ fromIntegral (word `shiftR` 32)
    low  = encode32 $ fromIntegral (word .&. 0xffffffff)

encodeWithSep64 :: Char -> Word64 -> B8.ByteString
encodeWithSep64 sep word = B8.append high $ B8.cons sep low
  where
    high = encodeWithSep32 sep $ fromIntegral (word `shiftR` 32)
    low  = encodeWithSep32 sep $ fromIntegral (word .&. 0xffffffff)

decode16 :: B8.ByteString -> Word16
decode16 proquint = if B8.isPrefixOf "0q-" proquint
                      then B8.foldl' decode 0 $ B8.take 5 $ B8.drop 3 proquint
                      else B8.foldl' decode 0 $ B8.take 5 proquint
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

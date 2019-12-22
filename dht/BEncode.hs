module BEncode
  ( decode
  , encode
  , BType(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Word (Word8)

data BType
  = BString B.ByteString
  | BInt Int
  | BList [BType]
  | BDict (M.Map B.ByteString BType)
  deriving (Eq, Show)

decode :: B.ByteString -> Maybe BType
decode b = fmap fst $ decodeBType b

encode :: BType -> BL.ByteString
encode obj = BB.toLazyByteString $ bTypeBuilder obj

-- Encode helpers
bTypeBuilder :: BType -> BB.Builder
bTypeBuilder (BString bs) = builderForBString bs
bTypeBuilder (BInt i) =
  (BB.char8 'i') <> (BB.byteString $ C.pack $ show i) <> (BB.char8 'e')
bTypeBuilder (BList items) =
  (foldl (\a b -> a <> b) (BB.char8 'l') (fmap bTypeBuilder items)) <>
  (BB.char8 'e')
bTypeBuilder (BDict items) =
  (foldl (\a b -> a <> b) (BB.char8 'd') (fmap builderForKV $ M.toList items)) <>
  (BB.char8 'e')
  where
    builderForKV (k, v) = builderForBString k <> bTypeBuilder v

builderForBString :: B.ByteString -> BB.Builder
builderForBString bs =
  (BB.string8 $ show $ B.length bs) <> (BB.char8 ':') <> (BB.byteString bs)

-- Decode helpers
decodeBType :: B.ByteString -> Maybe (BType, B.ByteString)
decodeBType b =
  case (C.uncons b) of
    Just ('l', _) -> decodeBList b
    Just ('i', _) -> decodeBInt b
    Just ('d', _) -> decodeBDict b
    Just _ -> decodeBString b
    _ -> Nothing

decodeBString :: B.ByteString -> Maybe (BType, B.ByteString)
decodeBString b = fmap mapBString $ decodeString b
  where
    mapBString (s, r) = (BString s, r)

decodeBInt :: B.ByteString -> Maybe (BType, B.ByteString)
decodeBInt bytes = fmap mapRes $ (convertInt . extractInt) bytes
  where
    extractInt = C.span (/= 'e') . B.drop 1
    convertInt (b, r) = fmap (\i -> (i, r)) $ toInt b
    mapRes (i, r) = (BInt i, B.drop 1 r)

decodeBDict :: B.ByteString -> Maybe (BType, B.ByteString)
decodeBDict bytes = mapRes $ accKV M.empty (B.drop 1 bytes)
  where
    accKV ::
         M.Map B.ByteString BType
      -> B.ByteString
      -> (M.Map B.ByteString BType, B.ByteString)
    accKV content b =
      case nextKV b of
        Nothing -> (content, b)
        Just (k, v, r) -> accKV (M.insert k v content) r
    nextKV :: B.ByteString -> Maybe (B.ByteString, BType, B.ByteString)
    nextKV b = nothingIfEnd b >>= getKey >>= getValue
    nothingIfEnd :: B.ByteString -> Maybe B.ByteString
    nothingIfEnd b =
      C.uncons b >>=
      (\(c, _) ->
         if c == 'e'
           then Nothing
           else Just b)
    getKey = decodeString
    getValue (key, rest) = fmap (\(t, r) -> (key, t, r)) $ decodeBType rest
    mapRes (m, r) = Just (BDict m, (B.drop 1 r))

decodeBList :: B.ByteString -> Maybe (BType, B.ByteString)
decodeBList bytes = mapRes $ accV [] (B.drop 1 bytes)
  where
    accV l b =
      case (nothingIfEnd b >>= decodeBType) of
        Nothing -> (l, b)
        Just (t, r) -> accV (t : l) r
    nothingIfEnd b =
      C.uncons b >>=
      (\(c, _) ->
         if c == 'e'
           then Nothing
           else Just b)
    mapRes (l, r) = Just (BList $ reverse l, (B.drop 1 r))

decodeString :: B.ByteString -> Maybe (B.ByteString, B.ByteString)
decodeString bytes = fmap extractString $ (convertSize . extractSize) bytes
  where
    extractSize = C.span (/= ':')
    convertSize (s, r) = fmap (\i -> (i, r)) $ toInt s
    extractString (size, b) = B.splitAt size (B.drop 1 b)

-- Convert a ByteString to a Maybe Integer holding the value represented by the
-- ByteString
toInt :: B.ByteString -> Maybe Int
toInt b = fmap fromIntegral $ B.foldl toIntFolder (Just 0) b
  where
    toIntFolder :: Maybe Integer -> Word8 -> Maybe Integer
    toIntFolder Nothing _ = Nothing
    toIntFolder (Just acc) w =
      let i = toInteger w
       in if i >= 48 && i <= 57
            then Just (acc * 10 + i - 48)
            else Nothing

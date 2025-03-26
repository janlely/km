{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Encrypt
  ( encrypt
  , decrypt 
  , encodeBase64
  , decodeBase64
  , makeSecret
  ) where

import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types (BlockCipher(..), Cipher(..), IV, makeIV)
import           Crypto.Error (CryptoFailable(..), CryptoError(..))

import qualified Crypto.Random.Types as CRT

import           Data.ByteArray (ByteArray, convert)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Not required, but most general implementation
data Key c a where
  Key :: (BlockCipher c, ByteArray a) => a -> Key c a

-- | Generates a string of bytes (key) of a specific length for a given block cipher
-- genSecretKey :: forall m c a. (CRT.MonadRandom m, BlockCipher c, ByteArray a) => c -> Int -> m (Key c a)
-- genSecretKey _ = fmap Key . CRT.getRandomBytes

makeSecret :: forall m . (CRT.MonadRandom m) =>  m T.Text
makeSecret = do
  bytes :: ByteString <- CRT.getRandomBytes 32 
  return $ encodeBase64 bytes


-- | Generate a random initialization vector for a given block cipher
genRandomIV :: forall m c. (CRT.MonadRandom m, BlockCipher c) => c -> m (Maybe (IV c))
genRandomIV _ = do
  bytes :: ByteString <- CRT.getRandomBytes $ blockSize (undefined :: AES256)
  return $ makeIV bytes

-- | Initialize a block cipher
initCipher :: (BlockCipher c, ByteArray a) => Key c a -> Either CryptoError c
initCipher (Key k) = case cipherInit k of
  CryptoFailed e -> Left e
  CryptoPassed a -> Right a

-- 将 IV c 转换为 ByteString
ivToByteString :: BlockCipher c => IV c -> ByteString
ivToByteString iv = convert iv

encrypt' :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
encrypt' secretKey initIV msg =
  case initCipher secretKey of
    Left e -> Left e
    Right c -> Right $ ctrCombine c initIV msg

encrypt :: ByteString -> ByteString -> IO (ByteString, ByteString)
encrypt msg key = do
  mInitIV <- genRandomIV (undefined :: AES256)
  case mInitIV of
    Nothing -> error "Failed to generate and initialization vector."
    Just initIV -> do
      case encrypt' (Key key) initIV msg of
        Left err -> error $ show err
        Right eMsg -> return (eMsg, ivToByteString initIV)

decrypt :: ByteString -> ByteString -> ByteString -> ByteString
decrypt msg key iv =
  case makeIV iv :: Maybe (IV AES256) of 
    Nothing -> error "Failed to make initialization vector."
    Just iv' -> do
      case encrypt' (Key key) iv' msg of
        Left err -> error $ show err
        Right dMsg -> dMsg

-- ByteString 转 base64 Text
encodeBase64 :: ByteString -> T.Text
encodeBase64 = TE.decodeUtf8 . B64.encode

-- base64 Text 转回 ByteString
decodeBase64 :: T.Text -> ByteString
decodeBase64 txt =
  case (B64.decode . TE.encodeUtf8) txt of
    Left _ -> error "error decode base64"
    Right bs -> bs

  

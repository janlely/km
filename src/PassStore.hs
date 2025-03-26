{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PassStore 
    ( insertKey
    , queryKey
    , getKey
    , generateKey
    , listKeys
    , delKey
    ) where


import Key
import qualified Data.Text as T
import System.Random
import Data.Char

import Encrypt
import System.Directory (getHomeDirectory, doesFileExist)
import System.FilePath ((</>))
import System.IO (hFlush, stdout, hGetEcho, hSetEcho, stdin)
import Control.Exception (bracket_)

import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as MS
import qualified Data.Store as DS
import qualified Data.List as DL
import Data.Maybe (fromJust)

getDBPath :: IO FilePath
getDBPath = do
  homeDir <- getHomeDirectory
  return $ homeDir </> ".km/kmdb.bin"

getPasswordStore :: BS.ByteString -> IO PasswordStore
getPasswordStore key = do
  dbPath <- getDBPath
  exists <- doesFileExist dbPath
  if not exists
    then return MS.empty
    else do
      (iv, content) <- BS.splitAt 16 <$> BS.readFile dbPath
      let bin = decrypt content key iv
      DS.decodeIO bin
      

insertKey :: Input -> IO ()
insertKey input = do 
  pass <- to32 . TE.encodeUtf8 . T.pack <$> getPassword
  passStore <- getPasswordStore pass
  let i :: Int = fromJust $ DL.find (`MS.notMember` passStore) [1..]
  let entry = PasswordEntry i (_username input) (_password input) (_desc input)
  let newStore = MS.insert i entry passStore
  let bin = DS.encode newStore
  (bin', iv) <- encrypt bin pass
  dbPath <- getDBPath
  BS.writeFile dbPath (BS.append iv bin')


queryKey :: T.Text -> IO ()
queryKey keyWords = do 
  pass <- to32 . TE.encodeUtf8 . T.pack <$> getPassword
  passStore <- getPasswordStore pass
  let matches = DL.filter (T.isInfixOf keyWords . _notes . snd) (MS.toList passStore)
  mapM_ print matches
  

getKey :: Int -> IO ()
getKey i = do
  pass <- to32 . TE.encodeUtf8 . T.pack <$> getPassword
  passStore <- getPasswordStore pass
  case MS.lookup i passStore of
    Just pe -> putStrLn $ T.unpack (_username0 pe) ++ " | " ++ T.unpack (_password0 pe)
    _ -> putStrLn "找不到指定密码"

generateKey :: Config -> IO ()
generateKey (PASSWORD cfg) = do
    passwd <- generatePassword cfg
    insertKey (Input (_user cfg) passwd (_desc1 cfg))
    putStrLn $ T.unpack passwd
generateKey _ = error "unsupported config type"


generatePassword :: PasswordCfg -> IO T.Text
generatePassword cfg@(PasswordCfg len _ u l n s  _) = do
    gen <- newStdGen
    let ret = take len $ randomRs ('\33', '\126') gen
    if all (\f -> f ret)
        [ if u then any isUpper else not . any isUpper
        , if l then any isLower else not . any isLower
        , if n then any isNumber else not . any isNumber
        , if s then not . all isAlphaNum else all isAlphaNum
        ]
        then return $ T.pack ret
        else generatePassword cfg

listKeys :: IO ()
listKeys = do
  pass <- to32 . TE.encodeUtf8 . T.pack <$> getPassword
  passStore <- getPasswordStore pass
  mapM_ print $ (fmap snd . MS.toList) passStore


delKey :: [Int] -> IO ()
delKey is = do
  pass <- to32 . TE.encodeUtf8 . T.pack <$> getPassword
  passStore <- getPasswordStore pass
  let newStore = DL.foldl (flip MS.delete) passStore is
  let bin = DS.encode newStore
  (bin', iv) <- encrypt bin pass
  dbPath <- getDBPath
  BS.writeFile dbPath (BS.append iv bin')


-- desensitize :: String -> String 
-- desensitize _ = replicate 10 '*'


-- makeSecretKey :: IO ()
-- makeSecretKey = do
--     pass <- to32 . TE.encodeUtf8 . T.pack <$> getPassword
--     secret <- makeSecret
--     (pass', iv) <- encrypt secret pass
--     homeDir <- getHomeDirectory
--     let keyPath = homeDir </> ".km/key.txt"
--     exists <- doesFileExist keyPath
--     when exists $ do 
--       putStr "密钥文件已存在，是否重新生成？[Yes] or [No]: "
--       answer <- getLine
--       when (answer /= "Yes") System.Exit.exitSuccess
--     appendFile keyPath $ T.unpack $ encodeBase64 pass'
--     appendFile keyPath getNativeNewline 
--     appendFile keyPath $ T.unpack $ encodeBase64 iv
--     putStrLn "密钥创建成功"
--   where  getNativeNewline :: String
--          getNativeNewline = case nativeNewline of
--                               CRLF -> "\r\n"  -- Windows 换行符
--                               LF   -> "\n"    -- Unix/Linux/macOS 换行符
    
to32 :: BS.ByteString -> BS.ByteString
to32 bs = let len = BS.length bs
           in if len >= 32
                then BS.take 32 bs
                else BS.append bs $ BS.replicate (32 - len) 0


-- getSecretKey :: IO BS.ByteString
-- getSecretKey = do
--     homeDir <- getHomeDirectory
--     let keyPath = homeDir </> ".km/key.txt"
--     exists <- doesFileExist keyPath
--     unless exists $ do
--         putStr "密钥文件不存在，是否创建？[Yes] or [No]: "
--         hFlush stdout
--         yes <- (== "Yes") <$> getLine
--         when yes makeSecretKey
--     pass <- to32 . TE.encodeUtf8 . T.pack <$> getPassword
--     handle <- openFile keyPath ReadMode
--     key <- decodeBase64 . T.pack <$> hGetLine handle
--     iv <- decodeBase64 . T.pack <$> hGetLine handle
--     return $ decrypt key pass iv 
    
    
    


getPassword :: IO String
getPassword = do
  putStr "请输入密码: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
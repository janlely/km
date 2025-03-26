{-# LANGUAGE OverloadedStrings #-}

module KeyStore 
    ( insertKey
    , insertKey'
    , queryKey
    , getKey
    , generateKey
    , listKeys
    , delKey
    , makeSecretKey
    , getSecretKey
    ) where

import Database.SQLite.Simple
import Key
import qualified Data.Text as T
import System.Random
import Data.Char

import System.Directory (getHomeDirectory, doesFileExist)
import System.FilePath ((</>))
import System.IO (openFile, hGetLine, IOMode(..), hFlush, stdout, hGetEcho, hSetEcho, stdin, nativeNewline, Newline(..))
import Control.Exception (bracket_)
import Control.Monad (forM_, when, unless)

import qualified Data.Text.Encoding as TE
import Encrypt (encrypt, decrypt, encodeBase64, decodeBase64, makeSecret)
import qualified System.Exit
import qualified Data.ByteString as BS
import GHC.Exception (getCallStack)



insertKey' :: BS.ByteString -> Input -> Connection -> IO ()
insertKey' bs input conn = do 
    (key', iv) <- encrypt (TE.encodeUtf8 $ _password input) bs
    execute conn "INSERT INTO key (username, password, iv, desc) VALUES (?,?,?,?)"
                        [_username input, encodeBase64 key', encodeBase64 iv, _desc input]


insertKey :: Input -> Connection -> IO ()
insertKey input conn = do 
    key <- getSecretKey
    (key', iv) <- encrypt (TE.encodeUtf8 $ _password input) key 
    execute conn "INSERT INTO key (username, password, iv, desc) VALUES (?,?,?,?)"
                        [_username input, encodeBase64 key', encodeBase64 iv, _desc input]

queryKey :: String -> Connection -> IO ()
queryKey keyWords conn = do 
    xs <- query conn "SELECT id, desc FROM key WHERE desc LIKE '%' || ? || '%'" [keyWords] 
    forM_ xs $ \(i,desc) ->
      putStrLn $ show (i :: Int) ++ " | " ++ T.unpack desc 

getKey :: Int -> Connection -> IO ()
getKey i conn = do
    xs <- query conn "SELECT username,password,iv FROM key WHERE id = ?" (Only (i :: Int)) :: IO [(T.Text, T.Text, T.Text)]
    -- homeDir <- getHomeDirectory
    -- let keyPath = homeDir </> ".km/key.txt"
    -- handle <- openFile keyPath ReadMode
    -- key <- T.pack <$> hGetLine handle
    key <- getSecretKey
    forM_ xs $ \(username,password, iv) ->
      putStrLn $ T.unpack username ++ " | " ++ (T.unpack . TE.decodeUtf8) (decrypt (decodeBase64 password) key (decodeBase64 iv))

generateKey :: Config -> Connection -> IO ()
generateKey (PASSWORD cfg) conn = do
    passwd <- generatePassword cfg
    insertKey (Input (_user cfg) passwd (_desc1 cfg)) conn
    -- execute conn "INSERT INTO key (username, password, iv, desc) VALUES (?,?,?,?)"
    --     [_user cfg, passwd, _desc1 cfg]
    putStrLn $ T.unpack passwd
generateKey _ _ = error "unsupported config type"


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

listKeys :: Connection -> IO ()
listKeys conn = do
    xs <- query_ conn "SELECT id,username,password,desc FROM key"  :: IO [(Int,T.Text, T.Text, T.Text)]
    forM_ xs $ \(i,username,password,desc) ->
      putStrLn $ show i ++ " | " ++ T.unpack username ++ " | " ++  desensitize (T.unpack password) ++ " | " ++ T.unpack desc


delKey :: [Int] -> Connection -> IO ()
delKey is conn = do
    let placeholders = mconcat (replicate (length is) "?,")
        queryStr = "DELETE FROM key WHERE id IN (" ++ init placeholders ++ ")"
    execute conn (Query (T.pack queryStr)) is
    putStrLn "OK"


desensitize :: String -> String 
desensitize _ = replicate 10 '*'


makeSecretKey :: IO ()
makeSecretKey = do
    pass <- to32 . TE.encodeUtf8 . T.pack <$> getPassword
    secret <- makeSecret
    (pass', iv) <- encrypt secret pass
    homeDir <- getHomeDirectory
    let keyPath = homeDir </> ".km/key.txt"
    exists <- doesFileExist keyPath
    when exists $ do 
      putStr "密钥文件已存在，是否重新生成？[Yes] or [No]: "
      answer <- getLine
      when (answer /= "Yes") System.Exit.exitSuccess
    appendFile keyPath $ T.unpack $ encodeBase64 pass'
    appendFile keyPath getNativeNewline 
    appendFile keyPath $ T.unpack $ encodeBase64 iv
    putStrLn "密钥创建成功"
  where  getNativeNewline :: String
         getNativeNewline = case nativeNewline of
                              CRLF -> "\r\n"  -- Windows 换行符
                              LF   -> "\n"    -- Unix/Linux/macOS 换行符
    
to32 :: BS.ByteString -> BS.ByteString
to32 bs = let len = BS.length bs
           in if len >= 32
                then BS.take 32 bs
                else BS.append bs $ BS.replicate (32 - len) 0


getSecretKey :: IO BS.ByteString
getSecretKey = do
    homeDir <- getHomeDirectory
    let keyPath = homeDir </> ".km/key.txt"
    exists <- doesFileExist keyPath
    unless exists $ do
        putStr "密钥文件不存在，是否创建？[Yes] or [No]: "
        hFlush stdout
        yes <- (== "Yes") <$> getLine
        when yes makeSecretKey
    pass <- to32 . TE.encodeUtf8 . T.pack <$> getPassword
    handle <- openFile keyPath ReadMode
    key <- decodeBase64 . T.pack <$> hGetLine handle
    iv <- decodeBase64 . T.pack <$> hGetLine handle
    return $ decrypt key pass iv 
    
    
    


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
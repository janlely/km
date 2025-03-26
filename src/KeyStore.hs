{-# LANGUAGE OverloadedStrings #-}

module KeyStore 
    ( insertKey
    , queryKey
    , getKey
    , generateKey
    , listKeys
    , delKey
    , makeSecretKey
    ) where

import Database.SQLite.Simple
import Key
import qualified Data.Text as T
import System.Random
import Data.Char

import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO (openFile, hGetLine, IOMode(..))
import Control.Monad (forM_)

import qualified Data.Text.Encoding as TE
import Encrypt (encrypt, decrypt, encodeBase64, decodeBase64, makeSecret)



insertKey :: Input -> Connection -> IO ()
insertKey input conn = do 
    homeDir <- getHomeDirectory
    let keyPath = homeDir </> ".km/key.txt"
    handle <- openFile keyPath ReadMode
    key <- decodeBase64 . T.pack <$> hGetLine handle
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
    homeDir <- getHomeDirectory
    let keyPath = homeDir </> ".km/key.txt"
    handle <- openFile keyPath ReadMode
    key <- T.pack <$> hGetLine handle
    forM_ xs $ \(username,password, iv) ->
      putStrLn $ T.unpack username ++ " | " ++ (T.unpack . TE.decodeUtf8) (decrypt (decodeBase64 password) (decodeBase64 key) (decodeBase64 iv))

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
makeSecretKey = makeSecret >>= putStrLn . T.unpack
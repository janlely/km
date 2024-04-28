{-# LANGUAGE OverloadedStrings #-}
module KeyStore 
    ( insertKey
    , queryKey
    , getKey
    , generateKey
    , listKeys
    , delKey
    ) where

import Database.SQLite.Simple
import Key
import qualified Data.Text as T
import Control.Monad
import System.Random
import Data.Char




insertKey :: Input -> Connection -> IO ()
insertKey input conn = execute conn "INSERT INTO key (username, password, desc) VALUES (?,?,?)"
                        [_username input, _password input, _desc input]

queryKey :: String -> Connection -> IO ()
queryKey keyWords conn = do 
    xs <- query conn "SELECT id, desc FROM key WHERE desc LIKE '%' || ? || '%'" [keyWords] 
    forM_ xs $ \(i,desc) ->
      putStrLn $ show (i :: Int) ++ " | " ++ T.unpack desc 

getKey :: Int -> Connection -> IO ()
getKey i conn = do
    xs <- query conn "SELECT username,password FROM key WHERE id = ?" (Only (i :: Int)) :: IO [(T.Text, T.Text)]
    forM_ xs $ \(username,password) ->
      putStrLn $ T.unpack username ++ " | " ++  T.unpack password

generateKey :: Config -> Connection -> IO ()
generateKey (PASSWORD cfg) conn = do
    passwd <- generatePassword cfg
    execute conn "INSERT INTO key (username, password, desc) VALUES (?,?,?)"
        [_user cfg, passwd, _desc1 cfg]
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
desensitize s = replicate (length s) '*'
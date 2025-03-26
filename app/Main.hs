{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Command
import Options.Applicative (execParser, info, helper, fullDesc, header, progDesc, (<**>))
import Database.SQLite.Simple
import KeyStore
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import qualified Data.Text as T
import Control.Monad (forM_)
import Key (Input(..))



-- import           Crypto.Cipher.AES (AES256)
-- import           Data.ByteString (ByteString)
-- import qualified Crypto.Random.Types as CRT
opts = info (cmdParser <**> helper) (fullDesc <> progDesc "Key Manager" <> header "km - a key manage tool")


main :: IO ()
main = do
    homeDir <- getHomeDirectory
    let dbPath = homeDir </> ".km/keys.db"
    conn <- open dbPath 
    cmd <- execParser opts
    execute_ conn "CREATE TABLE IF NOT EXISTS key (id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT, password TEXT, iv TEXT, desc TEXT);"
    case cmd of
        ADD input -> insertKey input conn
        QUERY keyWords -> queryKey keyWords conn
        GET i -> getKey i conn
        GENERATE cfg -> generateKey cfg conn
        LIST -> listKeys conn
        DEL i -> delKey i conn
        MAKESECRET -> makeSecretKey
    close conn 

-- main :: IO ()
-- main = do 
--   homeDir <- getHomeDirectory
--   let dbPath1 = homeDir </> ".km/key.db"
--   let dbPath2 = homeDir </> ".km/keys.db"
--   conn1 <- open dbPath1
--   conn2 <- open dbPath2
--   execute_ conn2 "CREATE TABLE IF NOT EXISTS key (id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT, password TEXT, iv TEXT, desc TEXT);"
--   xs <- query_ conn1 "SELECT id,username,password,desc FROM key"  :: IO [(Int,T.Text, T.Text, T.Text)]
--   key <- getSecretKey
--   forM_ xs $ \(i,username,password,desc) -> do
--     insertKey' key (Input username password desc) conn2 
  


-- main :: IO ()
-- main = do
--   key :: ByteString <- CRT.getRandomBytes 32 
--   (eMsg, iv) <- encrypt "hello jay" key
--   putStrLn $ "Message after encryption: " ++ show eMsg
--   putStrLn $ "Message after decryption: " ++ show (decrypt eMsg key iv)
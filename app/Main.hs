{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Command
import Options.Applicative (execParser, info, helper, fullDesc, header, progDesc, (<**>))
import Database.SQLite.Simple
import KeyStore
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))



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

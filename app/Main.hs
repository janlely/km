{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Command
import Options.Applicative (execParser, info, helper, fullDesc, header, progDesc, (<**>))
import Database.SQLite.Simple
import KeyStore
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

opts = info (cmdParser <**> helper) (fullDesc <> progDesc "Key Manager" <> header "km - a key manage tool")



main :: IO ()
main = do
    homeDir <- getHomeDirectory
    let filePath = homeDir </> ".km/key.db"
    conn <- open filePath 
    cmd <- execParser opts
    execute_ conn "CREATE TABLE IF NOT EXISTS key (id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT, password TEXT, desc TEXT);"
    case cmd of
        ADD input -> insertKey input conn
        QUERY keyWords -> queryKey keyWords conn
        GET i -> getKey i conn
        GENERATE cfg -> generateKey cfg conn
        LIST -> listKeys conn
    close conn 
    -- print =<< execParser opts

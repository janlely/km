{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Command
import Options.Applicative (execParser, info, helper, fullDesc, header, progDesc, (<**>))
import PassStore
import qualified Data.Text as T



opts = info (cmdParser <**> helper) (fullDesc <> progDesc "Key Manager" <> header "km - a key manage tool")


main :: IO ()
main = do
    -- homeDir <- getHomeDirectory
    -- let dbPath = homeDir </> ".km/keys.db"
    -- conn <- open dbPath 
    cmd <- execParser opts
    -- execute_ conn "CREATE TABLE IF NOT EXISTS key (id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT, password TEXT, iv TEXT, desc TEXT);"
    case cmd of
        ADD input -> insertKey input
        QUERY keyWords -> queryKey $ T.pack keyWords
        GET i -> getKey i
        GENERATE cfg -> generateKey cfg
        LIST -> listKeys
        DEL i -> delKey i

main :: IO ()
main = do
    homeDir <- getHomeDirectory
    let dbPath = homeDir </> ".km/key.db"
    conn <- open dbPath 
    

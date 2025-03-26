{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Command
import Options.Applicative (execParser, info, helper, fullDesc, header, progDesc, (<**>))
import PassStore


opts = info (cmdParser <**> helper) (fullDesc <> progDesc "Key Manager" <> header "km - a key manage tool")


main :: IO ()
main = do
    cmd <- execParser opts
    case cmd of
        ADD input -> insertKey input
        GET i -> getKey i
        GENERATE cfg -> generateKey cfg
        LIST -> listKeys
        DEL i -> delKey i

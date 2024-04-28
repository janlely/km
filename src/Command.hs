module Command
    ( Command(..)
    , cmdParser
    ) where

import Key (
    Input(..)
    -- , Output(..)
    , Config(..)
    , PasswordCfg(..)
    -- , ContentType(..)
    )
import qualified Options.Applicative as OA
import Options.Applicative ((<|>), (<**>))

data Command = ADD Input | QUERY String | GET Int | GENERATE Config | LIST deriving Show

-- typeParser :: OA.Parser ContentType
-- typeParser = OA.option OA.auto (OA.long "type"
--                                   <> OA.short 't'
--                                   <> OA.help "content type, R for RawText, H for Hexed string, B for Base64 string"
--                                   <> OA.metavar "<CONTENT TYPE>")


inputParser :: OA.Parser Input
-- inputParser = Input <$> typeParser
inputParser = Input <$> OA.strOption (OA.long "username"
                                             <> OA.short 'u'
                                             <> OA.help "username"
                                             <> OA.metavar "<USERNAME>")
                    <*> OA.strOption (OA.long "password"
                                        <> OA.short 'p'
                                        <> OA.help "password"
                                        <> OA.metavar "<PASSWORD>")
                    <*> OA.strOption (OA.long "description"
                                        <> OA.short 'd'
                                        <> OA.help "key description"
                                        <> OA.metavar "<DESCRIPTION>")

-- outputParser :: OA.Parser Output
-- outputParser = Output <$> OA.option OA.auto (OA.long "id" <> OA.help "key id" <> OA.metavar "<ID>")
--                       <*> typeParser

generateParser :: OA.Parser Config
generateParser = PASSWORD <$> passwordCfgParser 
  where passwordCfgParser = PasswordCfg <$> OA.argument OA.auto (OA.metavar "<LENGTH>" <> OA.help "length of the password")
                                        <*> OA.strOption (OA.long "username" <> OA.help "username" <> OA.metavar "<USERNAME>")
                                        <*> OA.switch (OA.long "upper-case" <> OA.short 'u' <> OA.help "with upper case required")
                                        <*> OA.switch (OA.long "lower-case" <> OA.short 'l' <> OA.help "with lower case required")
                                        <*> OA.switch (OA.long "number" <> OA.short 'n' <> OA.help "with number required")
                                        <*> OA.switch (OA.long "special" <> OA.short 's' <> OA.help "with special char required")
                                        <*> OA.strOption (OA.long "desc" <> OA.short 'd' <> OA.help "description" <> OA.metavar "<DESCRIPTION>")

cmdParser :: OA.Parser Command
cmdParser = OA.subparser (input <> query <> output <> generate <> list)
  where input = OA.command "add" (OA.info (ADD <$> inputParser <**> OA.helper) (OA.progDesc "input a new key"))
        query = OA.command "query" (OA.info (QUERY <$> OA.strArgument (OA.metavar "<KEY WORDS>")) (OA.progDesc "query key"))
        output = OA.command "get" (OA.info (GET <$> OA.argument OA.auto (OA.metavar "<id>") <**> OA.helper) (OA.progDesc "output the key"))
        generate = OA.command "generate" (OA.info (GENERATE <$> generateParser <**> OA.helper) (OA.progDesc "generate a new key"))
        list = OA.command "list" (OA.info (pure LIST) (OA.progDesc "list all keys"))
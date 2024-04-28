module Key
    ( Input(..)
    , Config(..)
    , PasswordCfg(..)
    ) where

import Data.Text (Text)


data Input = Input
    { _username :: Text
    , _password :: Text
    , _desc :: Text
    } deriving Show



data PasswordCfg = PasswordCfg
    { _length :: Int
    , _user :: Text
    , _uppercase :: Bool
    , _lower :: Bool
    , _number :: Bool
    , _special :: Bool
    , _desc1 :: Text
    } deriving Show

data Config = PASSWORD PasswordCfg | UNKNOWN deriving Show


-- {-# LANGUAGE TemplateHaskell #-}
module Key
    ( Input(..)
    -- , Output(..)
    , Config(..)
    -- , ContentType(..)
    , PasswordCfg(..)
    -- , inputType 
    ) where

import Data.Text (Text)


-- type FileExt = String

-- data ContentType = RawText | Hex | B64 deriving Show

-- instance Read ContentType where
--     readsPrec _ "R" = [(RawText,"")]
--     readsPrec _ "H" = [(Hex,"")]
--     readsPrec _ "B" = [(B64,"")]
--     readsPrec _ _ = error "unsupported ContentType"

data Input = Input
    { _username :: Text
    , _password :: Text
    , _desc :: Text
    } deriving Show

-- data Output = Output
--     { _id :: Int
--     , _type1 :: ContentType} deriving Show


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


-- makeLenses ''Input

-- inputType :: Input -> String
-- inputType input = show $ input^.contentType
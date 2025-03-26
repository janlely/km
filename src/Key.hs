{-# LANGUAGE DeriveGeneric #-}
module Key
    ( Input(..)
    , Config(..)
    , PasswordCfg(..)
    , PasswordEntry(..)
    , PasswordStore
    ) where

import Data.Text (Text, unpack)
import qualified Data.Map.Strict as MS
import Data.Store
import GHC.Generics


-- 密码条目
data PasswordEntry = PasswordEntry
    { _id :: Int
    , _username0 :: Text
    , _password0 :: Text -- 存储加密后的密码
    , _notes :: Text
    } deriving (Eq, Generic)

instance Store PasswordEntry

instance Show PasswordEntry where
    show (PasswordEntry i username _ notes) = show i ++ " | " ++ unpack username ++ " | " ++ unpack notes

-- 密码存储容器
type PasswordStore = MS.Map Int PasswordEntry
-- newtype PasswordStore = PasswordStore
--     { _store :: MS.Map Int PasswordEntry  -- 使用 Map 便于快速查找
--     } deriving (Show, Eq)

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


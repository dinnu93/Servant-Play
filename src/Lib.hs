{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

data SortBy = Age | Name

instance ToJSON User

issac :: User
issac =  User "Issac Newton" 372 "issac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "aemc2.org" (fromGregorian 1905 12 1)

users2 :: [User]
users2 = [issac, albert]

type UserAPI2 = "users" :> Get '[JSON] [User]
                :<|> "albert" :> Get '[JSON] User
                :<|> "issac" :> Get '[JSON] User

server2 :: Server UserAPI2
server2 = return users2
          :<|> return issac
          :<|> return albert

userAPI :: Proxy UserAPI2
userAPI = Proxy

app2 :: Application
app2 = serve userAPI server2

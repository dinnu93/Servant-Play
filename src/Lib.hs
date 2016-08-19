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

type API = "search" :> QueryParam "q" String :> Get '[JSON] [SearchResult]
           :<|> "url" :> ReqBody '[JSON] URL :> Post '[JSON] URL

data URL = URL { url :: String } deriving Generic

instance FromJSON URL
instance ToJSON URL

data SearchResult = SearchResult {urlResult :: String, textResult :: String} deriving Generic
instance ToJSON SearchResult


server :: Server API
server = search
         :<|> urlAddr
  where search :: Maybe String -> Handler [SearchResult]
        search Nothing = return []
        search (Just query) = return [SearchResult "http://google.com/" query]

        urlAddr :: URL -> Handler URL
        urlAddr = return 
          
genAPI :: Proxy API
genAPI = Proxy

app :: Application
app = serve genAPI server

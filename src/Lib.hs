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
import Data.Text
import Network.HTTP
import System.IO.Unsafe
import Control.Applicative
import qualified Database.Bloodhound as BH
import Data.Aeson
import Data.Either (Either(..))
import Data.Maybe (fromJust)
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (secondsToDiffTime, UTCTime(..))
import Data.Text 
import GHC.Generics (Generic)
import Network.HTTP.Client hiding (Proxy(..))
import qualified Network.HTTP.Types.Status as NHTS
import System.Random

-- Creating an Index with name SearchEngine

searchServer = BH.Server "http://localhost:9200"
searchIndex = BH.IndexName "url"
searchMapping = BH.MappingName "urlText"

data UrlTextMapping = UrlTextMapping deriving (Generic, Eq, Show)

instance ToJSON UrlTextMapping where
  toJSON UrlTextMapping = object ["urlText" .=
                                 object ["properties" .= 
                                         object ["urlRes" .= object ["type" .= ("string" :: Text)],
                                                 "textRes" .= object ["type" .= ("string" :: Text)]]]]

    
data UrlText = UrlText { urlRes :: Text
                       , textRes :: Text
                       } deriving (Generic, Eq, Show)

exampleURL = UrlText "http://google.com" "OK, Google!"

instance ToJSON UrlText
instance FromJSON UrlText

withBH' = BH.withBH defaultManagerSettings searchServer 

           

-------------------------------------------------------------------------- 

-- Helper Functions

indexURL :: URL -> IO BH.Reply
indexURL (URL u) = do uText <- simpleHTTP (getRequest u) >>= getResponseBody
                      rNumber <- randomRIO (1,10^20) :: IO Integer
                      let docId = show rNumber
                      let urlPost = UrlText (pack u) (pack uText)
                      withBH' (BH.indexDocument searchIndex searchMapping BH.defaultIndexDocumentSettings urlPost (BH.DocId $ pack docId))

-----------------------



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
        urlAddr u = liftIO (indexURL u) >> return u
          
genAPI :: Proxy API
genAPI = Proxy

app :: Application
app = serve genAPI server


        

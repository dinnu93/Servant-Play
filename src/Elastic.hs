module Elastic where 

{-# LANGUAGE DeriveGeneric #-}

import Control.Applicative
import Database.Bloodhound
import Data.Aeson
import Data.Either (Either(..))
import Data.Maybe (fromJust)
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (secondsToDiffTime, UTCTime(..))
import Data.Text 
import GHC.Generics (Generic)
import Network.HTTP.Client
import qualified Network.HTTP.Types.Status as NHTS

-- Creating an Index with name SearchEngine

searchServer = Server . pack $ "http://localhost:9200"
searchIndex = IndexName . pack $ "url"

withBH' = withBH defaultManagerSettings searchServer 
response = withBH' $ deleteIndex searchIndex


{-|
Module      : Profile.Live.Server.API.Connection
Description : Sub API about connections to haskell apps being profiled
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
module Profile.Live.Server.API.Bined(
    BinedGraph(..)
  , BinLine(..)
  -- * API 
  , BinedAPI 
  , binedAPI
  , binedOperations
  ) where

import Control.Lens
import Data.Colour.SRGB.Linear
import Data.Proxy 
import Data.Swagger 
import Data.Text 
import GHC.Generics
import Servant.API 
import Servant.API.Auth.Token 
import Servant.Swagger 
import Text.Read 

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU 
import qualified Data.Text as T 

import Profile.Live.Server.API.EventLog
import Profile.Live.Server.Utils.DeriveJson
import Profile.Live.Server.Utils.Schema

deriving instance Generic (RGB a)
$(deriveJSON (derivePrefix "channel") ''RGB)
instance ToSchema a => ToSchema (RGB a) where 
  declareNamedSchema = genericDeclareNamedSchema $
    schemaOptionsDropPrefix "channel"

instance ToParamSchema (RGB a) where 
  toParamSchema _ = mempty
    & type_ .~ SwaggerString
    & format .~ Just "RGB{channelRed=<r>,channelGreen=<g>,channelBlue=<b>}"

instance Read a => FromHttpApiData (RGB a) where
  parseQueryParam = either (Left . T.pack) Right . readEither . T.unpack

instance Show a => ToHttpApiData (RGB a) where 
  toQueryParam = T.pack . show 

-- | Single swimline of bined graph
data BinLine = BinLine {
  binLineName :: !Text -- ^ Name of thread
, binLineColour :: !(RGB Double) -- ^ Color of bins
, binLineOffset :: !Int -- ^ Starting bin
-- | Line content, each cell is defined by a floating number from 0 to 1 defining
-- a percent of thread workout during a bin
, binLineValues :: !(VU.Vector Double)
} deriving (Show, Generic)

$(deriveJSON (derivePrefix "binLine") ''BinLine)

instance ToSchema BinLine where 
  declareNamedSchema = genericDeclareNamedSchema $
    schemaOptionsDropPrefix "binLine"

-- | Data to draw bined graph
data BinedGraph = BinedGraph {
  binedGraphBegin :: !Double -- ^ Begin time of graph
, binedGraphEnd :: !Double -- ^ End time of graph
, binedGraphBinWidth :: !Double -- ^ Time width of bin
, binedGraphLines :: !(V.Vector BinLine) -- ^ Array of lines corresponding to thread
} deriving (Show, Generic)

$(deriveJSON (derivePrefix "binedGraph") ''BinedGraph)

instance ToSchema BinedGraph where 
  declareNamedSchema = genericDeclareNamedSchema $
    schemaOptionsDropPrefix "binedGraph"

-- | Sub API to work with bined graphs
type BinedAPI = "bined" :> (
       "full" 
    :> Capture "log-id" EventLogId
    :> QueryParam "binWidth" Double
    :> QueryParam "thread-colour" (RGB Double)
    :> QueryParam "gc-colour" (RGB Double)
    :> QueryParam "custom-colour" (RGB Double)
    :> TokenHeader' '["bined-graph"]
    :> Get '[JSON] BinedGraph
  )

-- | Proxy value to carry around 'BinedAPI' type
binedAPI :: Proxy BinedAPI 
binedAPI = Proxy 

-- | Select only operations of the Bined API
binedOperations :: Traversal' Swagger Operation
binedOperations = operationsOf $ toSwagger binedAPI
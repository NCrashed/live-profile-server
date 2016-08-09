module Profile.Live.Server.API.Connection(
    ConnectionAPI
  -- * Data types
  , Connection
  ) where 

import Data.Aeson.Unit
import Data.Aeson.WithField
import Data.Text 
import Data.Time 
import Data.Vinyl.Derived
import GHC.Generics 
import GHC.TypeLits 
import Servant.API 
import Servant.API.Auth.Token
import Servant.API.REST.Derive

-- | Connection to remote application
type Connection = FieldRec '[
    '("name", Text)
  , '("host", Text)
  , '("port", Word)
  , '("lastUsed", UTCTime)
  ]

type ConnectionAPI = "connection" :> RESTFull Connection "connection"
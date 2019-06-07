module DB.SimpleResult
    ( module DB.SimpleResult
    , module DB.Hasql
    , module Hasql.Decoders
    , module Hasql.Session
    , module Hasql.Statement
    , Text
    ) where

import           Data.Text
import           DB.Hasql
import qualified Hasql.Encoders as Encoders
import           Hasql.Decoders
import           Hasql.Session
import           Hasql.Statement
import           Data.ByteString

query' :: ByteString -> Result b -> Session b 
query' sqlCode decoder = statement () $ Statement sqlCode mempty decoder True

query :: ByteString -> Result a -> Handler (Maybe a)
query sqlCode decoder = mkQuery $ query' sqlCode decoder

queryParams :: ByteString -> a -> Encoders.Params a -> Result b -> Handler (Maybe b)
queryParams sqlCode val encoder decoder = mkQuery $ statement val $ Statement sqlCode encoder decoder True
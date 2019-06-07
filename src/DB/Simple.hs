module DB.Simple
    ( module DB.Simple
    , module DB.Hasql
    , module Hasql.Encoders
    , module Hasql.Session
    , module Hasql.Statement
    , Text
    ) where

import           Data.Text
import           DB.Hasql
import           Hasql.Encoders
import qualified Hasql.Decoders as Decoders
import           Hasql.Session
import           Hasql.Statement
import           Data.ByteString (ByteString)

query' :: ByteString -> a -> Params a -> Session ()
query' sqlCode val encoder = statement val $ Statement sqlCode encoder Decoders.unit True

query :: ByteString -> a -> Params a -> Handler (Maybe ())
query sqlCode val encoder = mkQuery $ query' sqlCode val encoder
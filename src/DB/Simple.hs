module DB.Simple
    ( module DB.Simple
    , module DB.Hasql
    , module Hasql.Encoders
    , module Hasql.Session
    , module Hasql.Statement
    , Text
    ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString        (ByteString)
import           Data.Text
import           DB.Hasql
import qualified Hasql.Decoders         as Decoders
import           Hasql.Encoders
import           Hasql.Session
import           Hasql.Statement

query' :: ByteString -> a -> Params a -> Session ()
query' sqlCode val encoder = statement val $ Statement sqlCode encoder Decoders.unit True

query :: MonadIO m => ByteString -> a -> Params a -> m (Maybe ())
query sqlCode val encoder = mkQuery $ query' sqlCode val encoder

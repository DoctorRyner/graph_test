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
import qualified Hasql.Encoders as E
import           Hasql.Decoders
import           Hasql.Session
import           Hasql.Statement
import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString

query' :: ByteString -> Result b -> Session b 
query' sqlCode decoder = statement () $ Statement sqlCode mempty decoder True

query :: MonadIO m => ByteString -> Result a -> m (Maybe a)
query sqlCode decoder = mkQuery $ query' sqlCode decoder

queryParams :: MonadIO m => ByteString -> a -> E.Params a -> Result b -> m (Maybe b)
queryParams sqlCode val encoder decoder = mkQuery $ statement val $ Statement sqlCode encoder decoder True
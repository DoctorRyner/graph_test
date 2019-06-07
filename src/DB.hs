module DB
    ( module DB
    , errOrUnpack
    , listArray
    , DB.SimpleResult.queryParams
    ) where

import           Data.ByteString
import           DB.Hasql
import qualified DB.Simple
import qualified DB.SimpleResult
import           Hasql.Decoders  as D
import           Hasql.Encoders  as E
import           Servant         (Handler)
import           Control.Monad.IO.Class (MonadIO)

query :: MonadIO m => ByteString -> Result a -> m (Maybe a)
query = DB.SimpleResult.query

exec :: MonadIO m => ByteString -> a -> Params a -> m (Maybe ())
exec = DB.Simple.query

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

query :: ByteString -> Result a -> Handler (Maybe a)
query = DB.SimpleResult.query

exec :: ByteString -> a -> Params a -> Handler (Maybe ())
exec = DB.Simple.query

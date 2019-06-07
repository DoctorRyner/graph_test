module DB.Hasql (module DB.Hasql, Handler) where

import           Control.Monad.IO.Class (liftIO)
import           Hasql.Connection
import           Hasql.Decoders         as D
import           Hasql.Session
import           Servant                (Handler, ServantErr, throwError)
import Control.Monad (replicateM)

dbSettings :: Settings
dbSettings = settings "localhost" 5432 "local" "" "graph_test"

connect :: IO Connection
connect = do
    Right connection <- acquire dbSettings
    pure connection

mkQuery :: Session a -> Handler (Maybe a)
mkQuery query = liftIO (run query =<< connect) >>= \case
    Right val -> pure $ Just val
    Left  _   -> pure Nothing

errOrUnpack :: ServantErr -> Handler (Maybe a) -> Handler a
errOrUnpack err handler = handler >>= \case
    Just val -> pure val
    Nothing  -> throwError err

-- listArray :: NullableOrNot Value element -> Value [element]
-- Library creators "encapsulated" that
listArray = D.array . D.dimension replicateM . D.element

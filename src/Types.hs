module Types where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Swagger (ToSchema)
import           Data.Text    (Text)
import           GHC.Generics (Generic)
import           Prelude      hiding (id)

data Node = Node
    { id    :: Int
    , links :: [Int]
    , label :: Text
    } deriving Generic

instance ToSchema Node
instance ToJSON Node

type Graph = [Node]
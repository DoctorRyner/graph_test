module Main where

import qualified Data.ByteString.Char8      as BSChar8
import           Data.Functor.Contravariant ((>$<))
import           Data.Maybe                 (catMaybes)
import           Data.Text                  (Text)
import           DB
import qualified Hasql.Decoders             as D
import qualified Hasql.Encoders             as E
import qualified Network.Wai.Handler.Warp   as Warp
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic
import           Servant.Swagger            (toSwagger)
import           Servant.Swagger.UI
import           Types

decodeNode :: D.Row Node
decodeNode = (\nodeId links label -> Node (fromIntegral nodeId) (map fromIntegral links) label)
    <$> (D.column D.int4)
    <*> (D.column $ listArray D.int4)
    <*> (D.column D.text)

graphNodeHandler :: Handler Graph
graphNodeHandler = errOrUnpack err404 $
    query "select id, links, label from graph" $ D.rowList decodeNode

nodeGetQuery :: Int -> Handler (Maybe Node)
nodeGetQuery targetId = query
    ("select id, links, label from graph where id = " <> (BSChar8.pack $ show targetId))
    (D.singleRow decodeNode)

nodesGetByIdListQuery :: [Int] -> Handler (Maybe [Node])
nodesGetByIdListQuery targetIds = query
    ("select id, links, label from graph" <> idCostructor)
    (D.rowList decodeNode)
  where
    bs = BSChar8.pack . show
    idCostructor = case length targetIds of
        0 -> " where id = -1"
        1 -> " where id = " <> (bs $ head targetIds)
        _ -> " where id = "
            <> bs (head targetIds)
            <> foldr
                (\x res -> res <> " or id = " <> bs x)
                ""
                (tail targetIds)

graphNodeGetHandler :: Int -> Handler Node
graphNodeGetHandler = errOrUnpack err404 . nodeGetQuery

graphNodeDelHandler :: Int -> Handler ()
graphNodeDelHandler targetNodeId = nodeGetQuery targetNodeId >>= \case
    Just _ -> errOrUnpack err404 $
        exec
            "delete from graph where id = $1"
            (fromIntegral targetNodeId)
            (E.param E.int4)
    Nothing -> throwError err404

graphNodeAddHandler :: Text -> Handler Int
graphNodeAddHandler label = request >>= \case
    Just newNodeId -> pure $ fromIntegral newNodeId
    Nothing        -> throwError err404 -- I would use err500 instead cuz it's only possible reason
  where
    request = queryParams
        "insert into graph (label, links) values ($1, array[]::int[]) returning id"
        label
        (E.param E.text)
        (D.singleRow $ D.column D.int4)

graphNodeRenHandler :: Int -> Text -> Handler ()
graphNodeRenHandler targetNodeId label = errOrUnpack err404 $
    exec
        "update graph set label = $1 where id = $2"
        (label, fromIntegral targetNodeId)
        (  (fst >$< E.param E.text)
        <> (snd >$< E.param E.int4)
        )

nodeLinkQuery :: Int -> Int -> Handler (Maybe ())
nodeLinkQuery id1 id2 = exec
    "update graph set links = links || $1 where id = $2"
    (fromIntegral id1, fromIntegral id2)
    (  (fst >$< E.param E.int4)
    <> (snd >$< E.param E.int4)
    )

graphNodeLinkHandler :: Int -> Int -> Handler ()
graphNodeLinkHandler idFrom idTo = if idFrom == idTo
    then throwError err400 -- Remove cycles
    else do
        maybeNodeFrom <- nodeGetQuery idFrom
        maybeNodeTo   <- nodeGetQuery idTo

        case (maybeNodeFrom, maybeNodeTo) of
            (Just nodeFrom, Just nodeTo) -> checkMultiplicity nodeFrom nodeTo
            _                            -> throwError err400

  where
    checkMultiplicity :: Node -> Node -> Handler () -- Remove multiplicity
    checkMultiplicity nodeFrom nodeTo = if elem idTo nodeFrom.links || elem idFrom nodeTo.links
        then throwError err400
        else do -- link two nodes by adding id to each other
            errOrUnpack err400 $ nodeLinkQuery idFrom idTo
            errOrUnpack err400 $ nodeLinkQuery idTo idFrom

graphNodeNeighboursHandler :: Int -> Handler [Node]
graphNodeNeighboursHandler targetId = do
    maybeTargetNode <- nodeGetQuery targetId

    case maybeTargetNode of
        Just targetNode -> errOrUnpack err400 $ nodesGetByIdListQuery targetNode.links
        Nothing         -> throwError err404

data Routes route = Routes
    { graphNode    :: route
        :- Summary "Get all nodes"
        :> "graph" :> "node" :> Get '[JSON] Graph
    , graphNodeGet :: route
        :- Summary "Get node by {id}"
        :> "graph" :> "node" :> Capture "id" Int :> Get '[JSON] Node
    , graphNodeAdd :: route
        :- Summary "Add node with {label} and returns it's {id}"
        :> "graph" :> "node" :> Capture "label" Text :> Put '[JSON] Int
    , graphNodeDel :: route
        :- Summary "Delete node with {id}"
        :> "graph" :> "node" :> Capture "id" Int :> Delete '[JSON] ()
    , graphNodeRename :: route
        :- Summary "Rename node with {id} using {label}"
        :> "graph" :> "node" :> Capture "id" Int :> Capture "label" Text :> Put '[JSON] ()
    , graphNodeLink :: route
        :- Summary "Links node by {id_to} with another node by {id_from}"
        :> "graph"
        :> "node"
        :> "link"
        :> Capture "id_from" Int
        :> Capture "id_to" Int
        :> Put '[JSON] ()
    , graphNodeNeighbours :: route
        :- Summary "Get all neighbours (linked nodes) from node wih {id}"
        :> "graph" :> "node" :> Capture "id" Int :> "neighbours" :> Get '[JSON] [Node]
    } deriving Generic

routes :: Routes AsServer
routes = Routes
    { graphNode           = graphNodeHandler
    , graphNodeGet        = graphNodeGetHandler
    , graphNodeAdd        = graphNodeAddHandler
    , graphNodeDel        = graphNodeDelHandler
    , graphNodeRename     = graphNodeRenHandler
    , graphNodeLink       = graphNodeLinkHandler
    , graphNodeNeighbours = graphNodeNeighboursHandler
    }

type API = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> ToServantApi Routes

server :: Server API
server = swaggerSchemaUIServer (toSwagger (Proxy :: Proxy (ToServantApi Routes)))
    :<|> toServant routes

main :: IO ()
main = Warp.run 3000 $ serve (Proxy :: Proxy API) server

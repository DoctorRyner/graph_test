# graph test

## How to build

(IMPORTANT read DB.Hasql overview to get psql credits)

Just type `stack build record-dot-preprocessor --copy-compiler-tool && stack run` and go to http://localhost:3000/swagger-ui

You need to install record-dot-preprocessor via copy-compiler-tool or you can install it globaly using `stack install record-dot-preprocessor`. You need it to allow accesing a record fileds using dot like `point.x + 2`

# Brief overview

## Types
Contains basic types for this task. I chose `Graph` to be just an alias to `[Node]` where Node is just a record, there is the declaration

```Haskell
data Node = Node
    { id    :: Int
    , links :: [Int]
    , label :: Text
    } deriving Generic

type Graph = [Node]
```

where `id` is obvious, `links` are ids of connected nodes and `label` is just a plain text label.

There are exist much more neat sollutions (to architect the Graph) but that was the easiest

## DB
This module is just an easy interface to the DB modules family. There are 2 functions

1. `query` should be used when you need to extract some result out of a query without encoding anything
2. `exec` should be used when you just need to execute some query and abandon a result
3. `queryParams` you also can export this from DB.SimpleResult if you need not only to extract some value but encode something into a query

## DB.Hasql
Contains basic DB stuff like connection info and helper functions

```Haskell
dbSettings :: Settings
dbSettings = settings "localhost" 5432 "local" "" "graph_test"
```

There are our credits to the DB. It means that we must have psql working on the `5432` port and have a user `local` with a db named `graph_test` with appropriate right (tabel will be created automatically if it does not exist)

## DB.Simple DB.SimpleResult
Contains helpers which just send query without a result and with a result accordingly

## Main
The server starts on the 3000 port in the main function.

`tableCheck` function checks if there is a table graph if not it creates it.

Type `API` just encompases Routes generic servant type and Swagger integration so do the `server` value.

And the app is simple, it just has type Routes with it realization (a value `routes`). A value `routes` has a record which filled with appropriate handlers which are triggered when appropriate route is encountered
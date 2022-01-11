module Postgrest.Resource.Client exposing
    ( Client
    , create
    , fetchMany
    , fetchOne
    , selects
    , update
    )

import Dict
import Postgrest.Client as PG exposing (Endpoint, Request, Selectable)
import Postgrest.Resource as Resource exposing (Resource)
import Postgrest.Schema exposing (Schema)
import Postgrest.Schema.Definition as Definition
    exposing
        ( Column(..)
        , Definition
        )
import Postgrest.Value as Value
import Url exposing (Url)
import Utils.Task exposing (Error(..))


type alias Client a =
    { a
        | schema : Schema
        , host : Url
        , jwt : PG.JWT
    }


fetchOne : Client a -> Definition -> String -> String -> Request Resource
fetchOne { host } definition resourcesName id =
    let
        pkName =
            Definition.primaryKeyName definition |> Maybe.withDefault ""
    in
    resourceEndpoint host resourcesName definition
        |> PG.getOne
        |> PG.setParams
            [ PG.select <| selects definition
            , PG.param pkName <| PG.eq <| PG.string id
            ]


fetchMany : Client a -> Definition -> String -> Request (List Resource)
fetchMany { host } definition resourcesName =
    resourceEndpoint host resourcesName definition
        |> Debug.log "many"
        |> PG.getMany
        |> PG.setParams [ PG.select <| selects definition ]


create : Client a -> Definition -> String -> Resource -> Request Resource
create { host } definition resourcesName resource =
    let
        endpoint =
            resourceEndpoint host resourcesName definition
    in
    Resource.encode resource
        |> PG.postOne endpoint
        |> PG.setParams [ PG.select <| selects definition ]


update :
    Client a
    -> Definition
    -> String
    -> String
    -> Resource
    -> Request Resource
update { host } definition resourcesName id resource =
    let
        pkName =
            Resource.primaryKeyName resource |> Maybe.withDefault ""

        endpoint =
            resourceEndpoint host resourcesName definition

        pk =
            PG.primaryKey ( pkName, PG.string )
    in
    Resource.encode resource
        |> PG.patchByPrimaryKey endpoint pk id
        |> PG.setParams
            [ PG.select <| selects definition ]


selects : Definition -> List Selectable
selects definition =
    let
        resources { table, labelColumnName } =
            labelColumnName
                |> Maybe.map
                    (\n -> PG.resource table <| PG.attributes [ n, "id" ])

        filteMapFun (Column _ val) =
            Value.foreignKeyParams val |> Maybe.andThen resources
    in
    Dict.values definition
        |> List.filterMap filteMapFun
        |> (++) (Dict.keys definition |> List.map PG.attribute)


resourceEndpoint : Url -> String -> Definition -> Endpoint Resource
resourceEndpoint url resourcesName definition =
    Resource.decoder definition
        |> PG.endpoint
            ({ url | path = "/" ++ resourcesName } |> Url.toString)

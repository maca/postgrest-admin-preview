module Postgrest.Record.Client exposing (create, fetchMany, fetchOne, update)

import Dict
import Postgrest.Client as PG exposing (Endpoint, Request, Selectable)
import Postgrest.Record as Record exposing (Record)
import Postgrest.Schema exposing (Schema)
import Postgrest.Schema.Definition as Definition
    exposing
        ( Column(..)
        , Definition
        )
import Postgrest.Value as Value
import Set
import Url.Builder as Url


type alias Client a =
    { a
        | host : String
        , schema : Schema
    }


fetchOne : Client a -> Definition -> String -> String -> Request Record
fetchOne { host, schema } definition resourcesName id =
    let
        pkName =
            Definition.primaryKeyName definition |> Maybe.withDefault ""
    in
    recordEndpoint host resourcesName definition
        |> PG.getOne
        |> PG.setParams
            [ PG.select <| selects schema definition
            , PG.param pkName <| PG.eq <| PG.string id
            ]


fetchMany : Client a -> Definition -> String -> Request (List Record)
fetchMany { host, schema } definition resourcesName =
    recordEndpoint host resourcesName definition
        |> PG.getMany
        |> PG.setParams [ PG.select <| selects schema definition ]


create : Client a -> Definition -> String -> Record -> Request Record
create { host, schema } definition resourcesName record =
    let
        endpoint =
            recordEndpoint host resourcesName definition
    in
    Record.encode record
        |> PG.postOne endpoint
        |> PG.setParams
            [ PG.select <| selects schema definition ]


update : Client a -> Definition -> String -> String -> Record -> Request Record
update { host, schema } definition resourcesName id record =
    let
        pkName =
            Record.primaryKeyName record |> Maybe.withDefault ""

        endpoint =
            recordEndpoint host resourcesName definition

        pk =
            PG.primaryKey ( pkName, PG.string )
    in
    Record.encode record
        |> PG.patchByPrimaryKey endpoint pk id
        |> PG.setParams
            [ PG.select <| selects schema definition ]


selects : Schema -> Definition -> List Selectable
selects schema definition =
    let
        mapFun name =
            Dict.keys
                >> Set.fromList
                >> Set.intersect (recordIdentifiers |> Set.fromList)
                >> Set.toList
                >> PG.attributes
                >> PG.resource name

        resources ( name, _ ) =
            Dict.get name schema |> Maybe.map (mapFun name)

        filteMapFun (Column _ val) =
            Value.foreignKeyReference val
                |> Maybe.andThen resources
    in
    Dict.values definition
        |> List.filterMap filteMapFun
        |> (++) (Dict.keys definition |> List.map PG.attribute)


recordEndpoint : String -> String -> Definition -> Endpoint Record
recordEndpoint host resourcesName definition =
    Record.decoder recordIdentifiers definition
        |> PG.endpoint (Url.crossOrigin host [ resourcesName ] [])


recordIdentifiers : List String
recordIdentifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]

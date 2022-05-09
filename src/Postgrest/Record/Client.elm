module Postgrest.Record.Client exposing
    ( Client
    , delete
    , fetch
    , fetchMany
    , jwtString
    , save
    , selects
    )

import Dict
import Dict.Extra as Dict
import Postgrest.Client as PG exposing (Endpoint, Request, Selectable)
import Postgrest.Field as Field
import Postgrest.Record as Record exposing (Record)
import Postgrest.Schema exposing (Column, Constraint(..), Schema, Table)
import PostgrestAdmin.AuthScheme as AuthScheme exposing (AuthScheme)
import Task exposing (Task)
import Url exposing (Url)
import Utils.Task exposing (Error(..))


type alias Client a =
    { a
        | schema : Schema
        , host : Url
        , authScheme : AuthScheme
    }


fetch : Client a -> Table -> String -> Task Error Record
fetch { host, authScheme } table id =
    case AuthScheme.toJwt authScheme of
        Just token ->
            case tablePrimaryKeyName table of
                Just primaryKeyName ->
                    resourceEndpoint host table
                        |> PG.getOne
                        |> PG.setParams
                            [ PG.select (selects table)
                            , PG.eq (PG.string id) |> PG.param primaryKeyName
                            ]
                        |> PG.toTask token
                        |> Task.mapError PGError

                Nothing ->
                    Task.fail (RequestError "No primary id in table")

        Nothing ->
            Task.fail AuthError


fetchMany : Client a -> Table -> Request (List Record)
fetchMany { host } table =
    resourceEndpoint host table
        |> PG.getMany
        |> PG.setParams [ PG.select <| selects table ]


save : Client a -> Table -> Maybe String -> Record -> Task Error Record
save client table maybeId record =
    case maybeId of
        Just id ->
            update client table id record

        Nothing ->
            create client table record


create : Client a -> Table -> Record -> Task Error Record
create { host, authScheme } table resource =
    case AuthScheme.toJwt authScheme of
        Just token ->
            let
                endpoint =
                    resourceEndpoint host table
            in
            Record.encode resource
                |> PG.postOne endpoint
                |> PG.setParams [ PG.select <| selects table ]
                |> PG.toTask token
                |> Task.mapError PGError

        Nothing ->
            Task.fail AuthError


update : Client a -> Table -> String -> Record -> Task Error Record
update { host, authScheme } table recordId record =
    case AuthScheme.toJwt authScheme of
        Just token ->
            case tablePrimaryKeyName table of
                Just primaryKeyName ->
                    let
                        endpoint =
                            resourceEndpoint host table

                        primaryKey =
                            PG.primaryKey ( primaryKeyName, PG.string )
                    in
                    Record.encode record
                        |> PG.patchByPrimaryKey endpoint primaryKey recordId
                        |> PG.setParams [ PG.select <| selects table ]
                        |> PG.toTask token
                        |> Task.mapError PGError

                Nothing ->
                    Task.fail (RequestError "No primary id in table")

        Nothing ->
            Task.fail AuthError


delete : Client a -> Table -> Record -> Task Error String
delete { host, authScheme } table record =
    case AuthScheme.toJwt authScheme of
        Just token ->
            case
                Maybe.map2 Tuple.pair
                    (tablePrimaryKeyName table)
                    (Record.id record)
            of
                Just ( primaryKeyName, recordId ) ->
                    let
                        endpoint =
                            resourceEndpoint host table

                        primaryKey =
                            PG.primaryKey ( primaryKeyName, PG.string )
                    in
                    PG.deleteByPrimaryKey endpoint primaryKey recordId
                        |> PG.toTask token
                        |> Task.mapError PGError

                Nothing ->
                    Task.fail (RequestError "No primary id in table")

        Nothing ->
            Task.fail AuthError


selects : Table -> List Selectable
selects table =
    Dict.values table.columns
        |> List.filterMap associationJoin
        |> (++) (Dict.keys table.columns |> List.map PG.attribute)


associationJoin : Column -> Maybe Selectable
associationJoin { constraint } =
    case constraint of
        ForeignKey { tableName, labelColumnName } ->
            labelColumnName
                |> Maybe.map
                    (\n -> PG.resource tableName (PG.attributes [ n, "id" ]))

        _ ->
            Nothing


resourceEndpoint : Url -> Table -> Endpoint Record
resourceEndpoint url table =
    Record.decoder table
        |> PG.endpoint ({ url | path = "/" ++ table.name } |> Url.toString)


jwtString : Client a -> Maybe String
jwtString { authScheme } =
    AuthScheme.toJwt authScheme
        |> Maybe.map PG.jwtString


tablePrimaryKeyName : Table -> Maybe String
tablePrimaryKeyName table =
    Dict.find (\_ column -> Field.isPrimaryKey column) table.columns
        |> Maybe.map Tuple.first

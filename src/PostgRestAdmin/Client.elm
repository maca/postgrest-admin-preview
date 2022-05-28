module PostgRestAdmin.Client exposing
    ( Client
    , toHostUrl
    , Table
    , getTable
    , fetchRecord
    , fetchRecordList
    , saveRecord
    , deleteRecord
    , Error
    , errorToString
    , expectRecord
    , expectRecordList
    , isAuthenticated
    , toJwtString
    )

{-|


# Client

@docs Client
@docs toHostUrl


# Table

@docs Table
@docs getTable


# Requests

Note that the request functions **do not produce a vanilla Elm
[Cmd](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#Cmd)**
but a [PostgRestAdmin.Cmd](PostgRestAdmin.Cmd).

@docs fetchRecord
@docs fetchRecordList
@docs saveRecord
@docs deleteRecord

@docs Error
@docs errorToString


# Result interpreting

@docs expectRecord
@docs expectRecordList


# Authentication

@docs isAuthenticated
@docs toJwtString

-}

import Internal.Client as Client
import Internal.Cmd as Internal
import Internal.Schema as Schema
import Json.Decode as Decode exposing (Decoder, Value)
import PostgRestAdmin.Cmd as AppCmd
import PostgRestAdmin.Record as Record exposing (Record)
import Postgrest.Client as PG
import Url exposing (Url)
import Utils.Task as Internal exposing (Error)


{-| Represents a client for a PostgREST instance, including authentication
params.

See [Config](PostgRestAdmin.Config) and
[Config.FormAuth](PostgRestAdmin.Config.FormAuth) for authentication configuration
options.

-}
type alias Client =
    Client.Client


{-| Represents a PostgREST table.
-}
type alias Table =
    Schema.Table


{-| Request error.
-}
type alias Error =
    Internal.Error


{-| Obtain the PostgREST instance url.
-}
toHostUrl : Client -> Url
toHostUrl =
    Client.toHostUrl


{-| Does the client has a valid JWT?
-}
isAuthenticated : Client -> Bool
isAuthenticated =
    Client.isAuthenticated


{-| Obtain the JWT as a string.
-}
toJwtString : Client -> Maybe String
toJwtString client =
    Client.toJwtString client


{-| Obtain a table from the table name.
-}
getTable : String -> Client -> Maybe Table
getTable =
    Client.getTable


{-| Transform [Error](#Error) to an error explanation.
-}
errorToString : Error -> String
errorToString =
    Internal.errorToString



-- VIEW


{-| Fetches a record for a given table.
`expect` param requires a function that returns a `Msg`.

You can use [expectRecord](#expectRecord) to interpret the result as a
[Record](PostgRestAdmin.Record).

    import PostgRestAdmin.Cmd as AppCmd

    fetch : String -> Client -> AppCmd.Cmd Msg
    fetch tableName client =
        case getTable tableName client of
            Just table ->
                fetchRecordList
                    { client = client
                    , table = table
                    , params = []
                    , expect = Client.expectRecord MyFetchedMsg table
                    }

            Nothing ->
                AppCmd.none

-}
fetchRecord :
    { client : Client
    , table : Table
    , id : String
    , expect : Result Error Value -> msg
    }
    -> AppCmd.Cmd msg
fetchRecord { client, table, expect, id } =
    Internal.Fetch expect (Client.fetchRecord client table id)


{-| Fetches a list of records for a given table.
`expect` param requires a function that returns a `Msg`.

You can use [expectRecordList](#expectRecordList) to interpret the result as a
list of [Record](PostgRestAdmin.Record)s.

    import PostgRestAdmin.Cmd as AppCmd

    fetchList : String -> Client -> AppCmd.Cmd Msg
    fetchList tableName client =
        case getTable tableName client of
            Just table ->
                fetchRecordList
                    { client = client
                    , table = table
                    , params = []
                    , expect = Client.expectRecordList MyListFetchedMsg table
                    }

            Nothing ->
                AppCmd.none

-}
fetchRecordList :
    { client : Client
    , table : Table
    , params : PG.Params
    , expect : Result Error Value -> msg
    }
    -> AppCmd.Cmd msg
fetchRecordList { client, table, params, expect } =
    Internal.Fetch expect (Client.fetchRecordList client table params)


{-| Saves a record.
`expect` param requires a function that returns a `Msg`.

You can use [expectRecord](#expectRecord) to interpret the result as a
[Record](PostgRestAdmin.Record).

    import PostgRestAdmin.Cmd as AppCmd

    save : Record -> Maybe String -> Client -> AppCmd.Cmd Msg
    save record id client =
        saveRecord
            { client = client
            , record = record
            , id = id
            , expect =
                Client.expectRecord MySavedMsg
                    (Record.getTable record)
            }

-}
saveRecord :
    { client : Client
    , record : Record
    , id : Maybe String
    , expect : Result Error Value -> msg
    }
    -> AppCmd.Cmd msg
saveRecord { client, record, id, expect } =
    Internal.Fetch expect (Client.saveRecord client record id)


{-| Deletes a record.
`expect` param requires a function that returns a `Msg`.

You can use [expectRecord](#expectRecord) to interpret the result as a
[Record](PostgRestAdmin.Record).

    import PostgRestAdmin.Cmd as AppCmd

    save : Record -> Client -> AppCmd.Cmd Msg
    save record client =
        deleteRecord
            { client = client
            , record = record
            , expect =
                Client.expectRecord MyDeletedMsg
                    (Record.getTable record)
            }

-}
deleteRecord :
    { client : Client
    , record : Record
    , expect : Result Error Value -> msg
    }
    -> AppCmd.Cmd msg
deleteRecord { client, record, expect } =
    Internal.Fetch expect (Client.deleteRecord client record)


{-| Decode the Value for successful Result as a record, decode as such and
map the success of the Result to a msg.

See [fetchRecord](#fetchRecord).

-}
expectRecord :
    (Result Error Record -> msg)
    -> Table
    -> (Result Error Value -> msg)
expectRecord tagger table =
    handleResponse (Record.decoder table) >> tagger


{-| Decode the Value for successful Result as a list of records, decode as such
and map the success of the Result to a msg.

See [fetchRecordList](#fetchRecordList).

-}
expectRecordList :
    (Result Error (List Record) -> msg)
    -> Table
    -> (Result Error Value -> msg)
expectRecordList tagger table =
    handleResponse (Decode.list (Record.decoder table)) >> tagger


handleResponse : Decoder a -> Result Error Value -> Result Error a
handleResponse decoder result =
    case result of
        Ok value ->
            Result.mapError Internal.DecodeError
                (Decode.decodeValue decoder value)

        Err err ->
            Err err

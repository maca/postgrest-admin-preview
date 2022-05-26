module PostgrestAdmin.Client exposing
    ( Client
    , toHostUrl
    , isAuthenticated
    , toJwtString
    , fetchRecord
    , fetchRecordList
    , saveRecord
    , deleteRecord
    , expectRecord
    , expectRecordList
    )

{-| Program configuration

@docs Client
@docs toHostUrl


# Authentication

@docs isAuthenticated
@docs toJwtString


# Requests

@docs fetchRecord
@docs fetchRecordList
@docs saveRecord
@docs deleteRecord


# Access

@docs expectRecord
@docs expectRecordList

-}

import Internal.Client as Client
import Internal.Cmd as AppCmd exposing (Cmd(..))
import Internal.Schema exposing (Table)
import Json.Decode as Decode exposing (Decoder, Value)
import Postgrest.Client as PG
import PostgrestAdmin.Record as Record exposing (Record)
import Url exposing (Url)
import Utils.Task exposing (Error(..))


{-| -}
type alias Client =
    Client.Client


{-| -}
toHostUrl : Client -> Url
toHostUrl =
    Client.toHostUrl


{-| -}
isAuthenticated : Client -> Bool
isAuthenticated =
    Client.isAuthenticated


{-| -}
toJwtString : Client -> Maybe String
toJwtString client =
    Client.toJwtString client



-- VIEW


{-| -}
fetchRecord :
    { client : Client
    , table : Table
    , id : String
    , expect : Result Error Value -> msg
    }
    -> AppCmd.Cmd msg
fetchRecord { client, table, expect, id } =
    Fetch expect (Client.fetchRecord client table id)


{-| -}
fetchRecordList :
    { client : Client
    , table : Table
    , params : PG.Params
    , expect : Result Error Value -> msg
    }
    -> AppCmd.Cmd msg
fetchRecordList { client, table, params, expect } =
    Fetch expect (Client.fetchRecordList client table params)


{-| -}
saveRecord :
    { client : Client
    , record : Record
    , id : Maybe String
    , expect : Result Error Value -> msg
    }
    -> AppCmd.Cmd msg
saveRecord { client, record, id, expect } =
    Fetch expect (Client.saveRecord client record id)


{-| -}
deleteRecord :
    { client : Client
    , record : Record
    , expect : Result Error Value -> msg
    }
    -> AppCmd.Cmd msg
deleteRecord { client, record, expect } =
    Fetch expect (Client.deleteRecord client record)


{-| -}
expectRecord :
    (Result Error Record -> msg)
    -> Table
    -> (Result Error Value -> msg)
expectRecord tagger table =
    handleResponse (Record.decoder table) >> tagger


{-| -}
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
            Result.mapError DecodeError (Decode.decodeValue decoder value)

        Err err ->
            Err err

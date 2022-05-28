module PostgRestAdmin.Record exposing
    ( Record
    , fromTable
    , id
    , label
    , errors
    , hasErrors
    , decoder
    , encode
    , getTable
    , tableName
    )

{-|


# Record

@docs Record
@docs fromTable
@docs id
@docs label


# Errors

@docs errors
@docs hasErrors


# Decode/Encode

@docs decoder
@docs encode


# Table

@docs getTable
@docs tableName

-}

import Dict exposing (Dict)
import Internal.Record as Record
import Internal.Schema exposing (Table)
import Json.Decode exposing (Decoder)
import Json.Encode as Encode


{-| A Record.
-}
type alias Record =
    Record.Record


{-| Create a blank record from a [Table](PostgRestAdmin.Client#Table).
-}
fromTable : Table -> Record
fromTable =
    Record.fromTable


{-| Get the [Table](PostgRestAdmin.Client#Table) for the Record.
-}
getTable : Record -> Table
getTable =
    Record.getTable


{-| Get the Record id.
-}
id : Record -> Maybe String
id =
    Record.id


{-| Get the Record [Table](PostgRestAdmin.Client#Table) name.
-}
tableName : Record -> String
tableName =
    Record.tableName


{-| Check if the Record has errors after [saving](PostgRestAdmin.Client#saveRecord).
-}
hasErrors : Record -> Bool
hasErrors =
    Record.hasErrors


{-| Get the Record errors after [saving](PostgRestAdmin.Client#saveRecord).
-}
errors : Record -> Dict String (Maybe String)
errors =
    Record.errors


{-| Encode a Record.
-}
encode : Record -> Encode.Value
encode =
    Record.encode


{-| A decoder for a Record.
-}
decoder : Table -> Decoder Record
decoder =
    Record.decoder


{-| Obtain a label identifying the record, could be the title or name attribute.
-}
label : Record -> Maybe String
label =
    Record.label

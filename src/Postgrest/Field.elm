module Postgrest.Field exposing
    ( Field
    , compareTuple
    , isPrimaryKey
    , setError
    , update
    , validate
    )

import List.Extra as List
import Postgrest.Client as PG
import Postgrest.Schema exposing (Constraint(..))
import Postgrest.Value as Value exposing (Value(..))


type alias Field =
    { constraint : Constraint
    , error : Maybe String
    , required : Bool
    , changed : Bool
    , value : Value
    }


isPrimaryKey : { a | constraint : Constraint } -> Bool
isPrimaryKey { constraint } =
    constraint == PrimaryKey


update : Value -> Field -> Field
update value field =
    validate { field | value = value, changed = True }


validate : Field -> Field
validate field =
    if field.required && Value.isNothing field.value then
        { field | error = Just "This field is required" }

    else
        { field | error = Nothing }


compareTuple :
    ( String, { a | constraint : Constraint } )
    -> ( String, { a | constraint : Constraint } )
    -> Order
compareTuple ( name, column ) ( name_, column_ ) =
    case ( column.constraint, column_.constraint ) of
        ( PrimaryKey, _ ) ->
            LT

        ( _, PrimaryKey ) ->
            GT

        ( ForeignKey _, _ ) ->
            LT

        ( _, ForeignKey _ ) ->
            GT

        _ ->
            compare (columnNameIndex name) (columnNameIndex name_)


setError : PG.PostgrestErrorJSON -> Field -> Field
setError { code } field =
    case code of
        Just "23502" ->
            { field | error = Just "This field is required" }

        _ ->
            field



-- Utils


columnNameIndex : String -> Int
columnNameIndex name =
    List.elemIndex name recordIdentifiers
        |> Maybe.withDefault (floor (1 / 0))


recordIdentifiers : List String
recordIdentifiers =
    [ "id", "email", "title", "name", "full name", "first name", "last name" ]

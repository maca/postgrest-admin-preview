module Postgrest.Field exposing (Field, compareTuple, setError, update, validate)

import List.Extra as List
import Postgrest.Client as PG
import Postgrest.Constraint as Constraint exposing (Constraint)
import Postgrest.Value as Value exposing (Value(..))


type alias Field =
    { constraint : Constraint
    , error : Maybe String
    , required : Bool
    , changed : Bool
    , value : Value
    }


update : Value -> Field -> Field
update value field =
    validate { field | value = value, changed = True }


validate : Field -> Field
validate field =
    if
        field.required
            && Value.isNothing field.value
            && (not <| Value.isPrimaryKey field.value)
    then
        { field | error = Just "This field is required" }

    else
        { field | error = Nothing }


compareTuple :
    ( String, { a | constraint : Constraint } )
    -> ( String, { a | constraint : Constraint } )
    -> Order
compareTuple ( name, column ) ( name_, column_ ) =
    case ( column.constraint, column_.constraint ) of
        ( Constraint.PrimaryKey, _ ) ->
            LT

        ( _, Constraint.PrimaryKey ) ->
            GT

        ( Constraint.ForeignKey _, _ ) ->
            LT

        ( _, Constraint.ForeignKey _ ) ->
            GT

        _ ->
            compare (columnNameIndex name) (columnNameIndex name_)


columnNameIndex : String -> Int
columnNameIndex name =
    List.elemIndex name recordIdentifiers
        |> Maybe.withDefault (floor (1 / 0))


setError : PG.PostgrestErrorJSON -> Field -> Field
setError { code } field =
    case code of
        Just "23502" ->
            { field | error = Just "This field is required" }

        _ ->
            field


recordIdentifiers : List String
recordIdentifiers =
    [ "id", "email", "title", "name", "full name", "first name", "last name" ]

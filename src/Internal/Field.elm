module Internal.Field exposing
    ( Field
    , compareTuple
    , isPrimaryKey
    , setError
    , toHtml
    , update
    , updateWithString
    , validate
    )

import Html exposing (Html, a, text)
import Html.Attributes exposing (href, target)
import Internal.Schema exposing (Constraint(..))
import Internal.Value as Value exposing (Value(..))
import List.Extra as List
import Postgrest.Client as PG
import String.Extra as String
import Time
import Time.Extra as Time
import Url.Builder as Url


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


updateWithString : String -> Field -> Field
updateWithString string field =
    update (Value.updateWithString string field.value) field


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



-- Html


toHtml : (String -> String -> Html.Attribute msg) -> String -> Field -> Html msg
toHtml onClick resourcesName { constraint, value } =
    case constraint of
        PrimaryKey ->
            recordLink resourcesName onClick value Nothing

        ForeignKey { tableName, label } ->
            recordLink tableName onClick value label

        NoConstraint ->
            valueToHtml value


valueToHtml : Value -> Html msg
valueToHtml value =
    case value of
        PFloat (Just float) ->
            text (String.fromFloat float)

        PInt (Just int) ->
            text (String.fromInt int)

        PString (Just string) ->
            text string

        PEnum (Just string) _ ->
            text (String.humanize string)

        PBool (Just True) ->
            text "true"

        PBool (Just False) ->
            text "false"

        PTime (Just time) ->
            text (Time.format time)

        PDate (Just time) ->
            text (Time.toDateString time)

        PText (Just string) ->
            text string

        Unknown _ ->
            text "?"

        _ ->
            text ""


recordLink :
    String
    -> (String -> String -> Html.Attribute msg)
    -> Value
    -> Maybe String
    -> Html msg
recordLink resourcesName onClick value mtext =
    let
        id =
            Value.toString value |> Maybe.withDefault ""
    in
    a
        [ href <| Url.absolute [ resourcesName, id ] []
        , target "_self"
        , onClick resourcesName id
        ]
        [ Maybe.map text mtext |> Maybe.withDefault (valueToHtml value) ]



-- Utils


columnNameIndex : String -> Int
columnNameIndex name =
    List.elemIndex name recordIdentifiers
        |> Maybe.withDefault (floor (1 / 0))


recordIdentifiers : List String
recordIdentifiers =
    [ "id", "email", "title", "name", "full name", "first name", "last name" ]

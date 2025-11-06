module Internal.Field exposing
    ( Field
    , compareTuple
    , isPrimaryKey
    , setValidation
    , toHtml
    , update
    , updateWithString
    , validate
    , valueToHtml
    )

import Html exposing (Html, a, pre, text)
import Html.Attributes exposing (href)
import Internal.Schema exposing (Constraint(..))
import Internal.Value as Value exposing (Value(..))
import List.Extra as List
import PostgRestAdmin.MountPath exposing (MountPath, path)
import String.Extra as String
import Time.Extra as Time
import Url.Builder as Url


type alias Field =
    { constraint : Constraint
    , required : Bool
    , value : Value
    , validation : Value -> Maybe String
    , changed : Bool
    , error : Maybe String
    }


isPrimaryKey : { a | constraint : Constraint } -> Bool
isPrimaryKey { constraint } =
    constraint == PrimaryKey


update : Value -> Field -> Field
update value field =
    validate False { field | value = value, changed = True }


updateWithString : String -> Field -> Field
updateWithString string field =
    update (Value.updateWithString string field.value) field


validate : Bool -> Field -> Field
validate persisted field =
    if isPrimaryKey field && not persisted then
        { field
            | error =
                field.validation field.value
        }

    else if field.required && Value.isNothing field.value then
        { field | error = Just "This field is required" }

    else
        { field | error = field.validation field.value }


setValidation : (Value -> Maybe String) -> Field -> Field
setValidation validation field =
    { field
        | validation =
            \value ->
                validation value
                    |> Maybe.map Just
                    |> Maybe.withDefault (field.validation value)
    }


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



-- Html


toHtml : MountPath -> String -> Field -> Html msg
toHtml mountPath resourcesName { constraint, value } =
    case constraint of
        PrimaryKey ->
            recordLink mountPath resourcesName value Nothing

        ForeignKey { tableName, label } ->
            recordLink mountPath tableName value label

        NoConstraint ->
            valueToHtml value


valueToHtml : Value -> Html msg
valueToHtml value =
    case value of
        PFloat maybe ->
            maybeToHtml String.fromFloat maybe

        PInt maybe ->
            maybeToHtml String.fromInt maybe

        PString maybe ->
            maybeToHtml identity maybe

        PEnum maybe _ ->
            maybeToHtml String.humanize maybe

        PBool maybe ->
            maybeToHtml
                (\bool ->
                    if bool then
                        "true"

                    else
                        "false"
                )
                maybe

        PTime maybe ->
            maybeToHtml Time.format maybe

        PDate maybe ->
            maybeToHtml Time.toDateString maybe

        PText maybe ->
            maybeToHtml identity maybe

        PJson _ ->
            pre
                []
                [ Value.toString value
                    |> maybeToHtml identity
                ]

        Unknown _ ->
            text "?"


maybeToHtml : (a -> String) -> Maybe a -> Html msg
maybeToHtml func maybe =
    Maybe.map func maybe |> Maybe.withDefault "" |> text


recordLink : MountPath -> String -> Value -> Maybe String -> Html msg
recordLink mountPath resourcesName value mtext =
    let
        id =
            Maybe.withDefault "" (Value.toString value)
    in
    a
        [ href (path mountPath (Url.absolute [ resourcesName, id ] []))
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

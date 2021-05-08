module Field exposing (Field, input, update, validate)

import Html exposing (..)
import Html.Attributes exposing (checked, for, id, type_, value)
import Html.Events exposing (onInput)
import Iso8601
import String.Extra as String
import Value exposing (Value(..))


type alias Field =
    { error : Maybe String
    , required : Bool
    , changed : Bool
    , value : Value
    }


update : Field -> String -> Field
update field string =
    validate
        { field
            | value = Value.update string field.value
            , changed = True
        }


input : (String -> Field -> a) -> String -> Field -> Html a
input tagger name ({ value } as field) =
    case value of
        PString maybe ->
            formInput tagger [] "text" name field maybe

        PFloat maybe ->
            Maybe.map String.fromFloat maybe
                |> formInput tagger [] "number" name field

        PInt maybe ->
            Maybe.map String.fromInt maybe
                |> formInput tagger [] "number" name field

        PBool maybe ->
            let
                attrs =
                    Maybe.map (checked >> List.singleton) maybe
                        |> Maybe.withDefault []
            in
            formInput tagger attrs "checkbox" name field Nothing

        PTime maybe ->
            Maybe.map (Iso8601.fromTime >> String.slice 0 19) maybe
                |> formInput tagger [] "datetime-local" name field

        _ ->
            text ""


formInput :
    (String -> Field -> a)
    -> List (Html.Attribute a)
    -> String
    -> String
    -> Field
    -> Maybe String
    -> Html a
formInput tagger attributes t name field mstring =
    let
        input_ =
            Html.input
                (attributes
                    ++ [ onInput <| (tagger name << update field)
                       , id name
                       , type_ t
                       , value <| Maybe.withDefault "" mstring
                       ]
                )
                []

        labelText =
            if field.required then
                String.humanize name ++ "*"

            else
                String.humanize name
    in
    div []
        [ label [ for name ] [ text <| labelText ]
        , input_
        ]


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

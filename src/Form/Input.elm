module Form.Input exposing (Input(..), Msg, display, field, updateRecord, value)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes
    exposing
        ( attribute
        , checked
        , class
        , classList
        , for
        , id
        )
import Html.Events exposing (onInput)
import Iso8601
import Maybe.Extra as Maybe
import Postgrest.Field as Field exposing (Field)
import Postgrest.Value exposing (Value(..))
import String.Extra as String


type alias Record =
    Dict String Input


type Msg
    = Changed String Input


type Input
    = Input Field


field : Input -> Field
field (Input inputField) =
    inputField


value : Input -> Value
value input =
    .value <| field input


updateRecord : Msg -> Record -> Record
updateRecord (Changed name changedInput) record =
    Dict.insert name changedInput record


update : String -> Input -> Input
update string input =
    Input <| Field.update string <| field input


display : String -> Input -> Html Msg
display name ((Input inputField) as input) =
    case inputField.value of
        PString maybe ->
            inputHelp name [] "text" input maybe

        PFloat maybe ->
            Maybe.map String.fromFloat maybe
                |> inputHelp name [] "number" input

        PInt maybe ->
            Maybe.map String.fromInt maybe
                |> inputHelp name [] "number" input

        PBool maybe ->
            let
                attrs =
                    Maybe.map checked maybe
                        |> Maybe.withDefault (attribute "" "")
                        |> List.singleton
            in
            inputHelp name attrs "checkbox" input Nothing

        PTime maybe ->
            Maybe.map (Iso8601.fromTime >> String.slice 0 19) maybe
                |> inputHelp name [] "datetime-local" input

        _ ->
            text ""


inputHelp :
    String
    -> List (Html.Attribute Msg)
    -> String
    -> Input
    -> Maybe String
    -> Html Msg
inputHelp name attributes type_ ((Input { required, error }) as input) mstring =
    let
        input_ =
            Html.input
                (attributes
                    ++ [ onInput <| (Changed name << flip update input)
                       , id name
                       , Html.Attributes.type_ type_
                       , Html.Attributes.value <| Maybe.withDefault "" mstring
                       ]
                )
                []

        labelText =
            if required then
                String.humanize name ++ "*"

            else
                String.humanize name

        errorText =
            error
                |> Maybe.map (text >> List.singleton >> p [ class "error" ])
                |> Maybe.withDefault (text "")
    in
    div
        [ class "field"
        , classList [ ( "with-error", Maybe.isJust error ) ]
        ]
        [ label [ for name ] [ text labelText ], input_, errorText ]

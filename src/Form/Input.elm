module Form.Input exposing (Input(..), Msg, display, field, updateRecord, value)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Form.Input.Autocomplete as Autocomplete exposing (Autocomplete)
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
    = Input Autocomplete Field


field : Input -> Field
field (Input _ inputField) =
    inputField


value : Input -> Value
value input =
    .value <| field input


updateRecord : Msg -> Record -> Record
updateRecord (Changed name changedInput) record =
    Dict.insert name changedInput record


update : String -> Input -> Input
update string (Input autocomplete inputField) =
    Input autocomplete <| Field.update string inputField


display : String -> Input -> Html Msg
display name ((Input _ inputField) as input) =
    case inputField.value of
        PString mstring ->
            displayInput "text" input mstring
                |> wrapInput input name

        PFloat mfloat ->
            Maybe.map String.fromFloat mfloat
                |> displayInput "number" input
                |> wrapInput input name

        PInt mint ->
            Maybe.map String.fromInt mint
                |> displayInput "number" input
                |> wrapInput input name

        PBool mbool ->
            displayCheckbox input mbool
                |> wrapInput input name

        PTime mtime ->
            Maybe.map (Iso8601.fromTime >> String.slice 0 19) mtime
                |> displayInput "datetime-local" input
                |> wrapInput input name

        _ ->
            text ""


wrapInput : Input -> String -> (String -> Html Msg) -> Html Msg
wrapInput ((Input _ { error }) as input) name buildInput =
    div
        [ class "field"
        , classList [ ( "with-error", Maybe.isJust error ) ]
        ]
        [ displayLabel input name, buildInput name, displayError error ]


displayLabel : Input -> String -> Html Msg
displayLabel (Input _ { required }) name =
    let
        labelText =
            if required then
                String.humanize name ++ "*"

            else
                String.humanize name
    in
    label [ for name ] [ text labelText ]


displayInput : String -> Input -> Maybe String -> String -> Html Msg
displayInput type_ input mstring name =
    Html.input
        [ onInput <| (Changed name << flip update input)
        , id name
        , Html.Attributes.type_ type_
        , Html.Attributes.value <| Maybe.withDefault "" mstring
        ]
        []


displayCheckbox : Input -> Maybe Bool -> String -> Html Msg
displayCheckbox input mchecked name =
    Html.input
        [ onInput <| (Changed name << flip update input)
        , id name
        , Html.Attributes.type_ "checkbox"
        , Maybe.map checked mchecked |> Maybe.withDefault (attribute "" "")
        ]
        []


displayError : Maybe String -> Html Msg
displayError error =
    error
        |> Maybe.map (text >> List.singleton >> p [ class "error" ])
        |> Maybe.withDefault (text "")

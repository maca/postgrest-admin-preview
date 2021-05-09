module Form.Input exposing (Input, input)

import Field exposing (Field)
import Html exposing (..)
import Html.Attributes exposing (attribute, checked, for, id, type_, value)
import Html.Events exposing (onInput)
import Iso8601
import String.Extra as String
import Value exposing (Value(..))


type alias Input a =
    { onChange : String -> Field -> a
    , name : String
    , attributes : List (Html.Attribute a)
    }


input : Input a -> Field -> Html a
input params field =
    case field.value of
        PString maybe ->
            inputHelp params "text" field maybe

        PFloat maybe ->
            Maybe.map String.fromFloat maybe
                |> inputHelp params "number" field

        PInt maybe ->
            Maybe.map String.fromInt maybe
                |> inputHelp params "number" field

        PBool maybe ->
            let
                attr =
                    Maybe.map checked maybe
                        |> Maybe.withDefault (attribute "" "")
            in
            inputHelp
                { params | attributes = attr :: params.attributes }
                "checkbox"
                field
                Nothing

        PTime maybe ->
            Maybe.map (Iso8601.fromTime >> String.slice 0 19) maybe
                |> inputHelp params "datetime-local" field

        _ ->
            text ""


inputHelp : Input a -> String -> Field -> Maybe String -> Html a
inputHelp { onChange, attributes, name } t field mstring =
    let
        input_ =
            Html.input
                (attributes
                    ++ [ onInput <| (onChange name << Field.update field)
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

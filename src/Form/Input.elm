module Form.Input exposing
    ( Input(..)
    , Msg
    , display
    , fromField
    , isRequired
    , setError
    , toField
    , toValue
    , update
    )

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
import Postgrest.Client exposing (PostgrestErrorJSON)
import Postgrest.Field as Field exposing (Field)
import Postgrest.Value as Value exposing (Value(..))
import String.Extra as String


type alias Record =
    Dict String Input


type Msg
    = Changed String Input


type Input
    = Text Field
    | Number Field
    | Checkbox Field
    | DateTime Field
    | Blank Field


toField : Input -> Field
toField input =
    case input of
        Text field ->
            field

        Number field ->
            field

        Checkbox field ->
            field

        DateTime field ->
            field

        Blank field ->
            field


fromField : Field -> Input
fromField field =
    case field.value of
        PString _ ->
            Text field

        PFloat _ ->
            Number field

        PInt _ ->
            Number field

        PBool _ ->
            Checkbox field

        PTime _ ->
            DateTime field

        PForeignKey _ _ _ ->
            Blank field

        PPrimaryKey _ ->
            Blank field

        BadValue _ ->
            Blank field


toValue : Input -> Value
toValue input =
    .value <| toField input


isRequired : Input -> Bool
isRequired input =
    .required <| toField input


setError : PostgrestErrorJSON -> Input -> Input
setError error input =
    toField input |> Field.setError error |> fromField


toError : Input -> Maybe String
toError input =
    .error <| toField input


update : Msg -> Record -> ( Record, Cmd Msg )
update msg record =
    case msg of
        Changed name input ->
            ( Dict.insert name input record, Cmd.none )


updateInput : String -> Input -> Input
updateInput string input =
    toField input |> Field.update string |> fromField


display : String -> Input -> Html Msg
display name input =
    case input of
        Text { value } ->
            Value.toString value
                |> displayInput "text" input
                |> wrapInput input name

        Number { value } ->
            Value.toString value
                |> displayInput "number" input
                |> wrapInput input name

        Checkbox { value } ->
            Value.isTrue value
                |> displayCheckbox input
                |> wrapInput input name

        DateTime { value } ->
            Value.toString value
                |> displayInput "datetime-local" input
                |> wrapInput input name

        _ ->
            text ""


wrapInput : Input -> String -> (String -> Html Msg) -> Html Msg
wrapInput input name buildInput =
    div
        [ class "field"
        , classList
            [ ( "with-error", Maybe.isJust <| toError input )
            , ( "required", isRequired input )
            ]
        ]
        [ label [ for name ] [ text <| String.humanize name ]
        , buildInput name
        , displayError <| toError input
        ]


displayInput : String -> Input -> Maybe String -> String -> Html Msg
displayInput type_ input mstring name =
    Html.input
        [ onInput <| (Changed name << flip updateInput input)
        , id name
        , if isRequired input then
            attribute "aria-required" "true"

          else
            class ""
        , Html.Attributes.type_ type_
        , Html.Attributes.value <| Maybe.withDefault "" mstring
        ]
        []


displayCheckbox : Input -> Maybe Bool -> String -> Html Msg
displayCheckbox input mchecked name =
    Html.input
        [ onInput <| (Changed name << flip updateInput input)
        , id name
        , Html.Attributes.type_ "checkbox"
        , Maybe.map checked mchecked |> Maybe.withDefault (class "")
        ]
        []


displayError : Maybe String -> Html Msg
displayError error =
    error
        |> Maybe.map (text >> List.singleton >> p [ class "error" ])
        |> Maybe.withDefault (text "")

module Form.Input exposing
    ( Input(..)
    , Msg
    , fromField
    , isRequired
    , setError
    , toField
    , toValue
    , update
    , view
    )

import Dict exposing (Dict)
import Error exposing (Error(..))
import Html exposing (..)
import Html.Attributes
    exposing
        ( attribute
        , checked
        , class
        , classList
        , for
        , id
        , list
        )
import Html.Events exposing (onInput)
import Maybe.Extra as Maybe
import Postgrest.Client as PG exposing (PostgrestErrorJSON)
import Postgrest.Field as Field exposing (Field)
import Postgrest.Resource exposing (Resource)
import Postgrest.Resource.Client as Client exposing (Client)
import Postgrest.Value as Value exposing (Value(..))
import Result
import String.Extra as String
import Task


type alias Record =
    Dict String Input


type alias AssociationParams =
    { resourcesName : String
    , resources : List Resource
    , userInput : Maybe String
    }


type Msg
    = Changed String Input String
    | AutocompleteInput String Field AssociationParams String
    | ListingFetched String Field AssociationParams (Result Error (List Resource))
    | Failure (Result Error Never)


type Input
    = Text Field
    | Number Field
    | Checkbox Field
    | DateTime Field
    | Association Field AssociationParams
    | Blank Field


update : Client a -> Msg -> Record -> ( Record, Cmd Msg )
update pgParams msg record =
    case msg of
        Changed name input value ->
            ( Dict.insert name (updateInput value input) record, Cmd.none )

        AutocompleteInput name field params value ->
            let
                input =
                    Association field { params | userInput = Just value }

                tagger =
                    ListingFetched name field params
            in
            ( Dict.insert name input record
            , fetchResources tagger params.resourcesName pgParams
            )

        ListingFetched name field params result ->
            case result of
                Ok resources ->
                    let
                        input =
                            Association field { params | resources = resources }
                    in
                    ( Dict.insert name input record, Cmd.none )

                Err _ ->
                    ( record, Cmd.none )

        Failure _ ->
            ( record, Cmd.none )


updateInput : String -> Input -> Input
updateInput string input =
    toField input |> Field.update string |> fromField


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

        Association field _ ->
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

        PForeignKey _ { table, label } ->
            Association field
                { resourcesName = table
                , userInput = label
                , resources = []
                }

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


view : String -> Input -> Html Msg
view name input =
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

        Association field params ->
            displayAutocompleteInput params field |> wrapInput input name

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


displayAutocompleteInput : AssociationParams -> Field -> String -> Html Msg
displayAutocompleteInput ({ userInput } as params) field name =
    let
        listName =
            "list-" ++ name
    in
    div []
        [ Html.input
            [ onInput <| AutocompleteInput name field params
            , id name
            , list listName
            , if field.required then
                attribute "aria-required" "true"

              else
                class ""
            , Html.Attributes.type_ "text"
            , Html.Attributes.value <| Maybe.withDefault "" userInput
            ]
            []
        ]


displayInput : String -> Input -> Maybe String -> String -> Html Msg
displayInput type_ input mstring name =
    Html.input
        [ onInput <| Changed name input
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
        [ onInput <| Changed name input
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


fetchResources :
    (Result Error (List Resource) -> Msg)
    -> String
    -> Client a
    -> Cmd Msg
fetchResources tagger resourcesName ({ schema, jwt } as client) =
    case Dict.get resourcesName schema of
        Just definition ->
            Client.fetchMany client definition resourcesName
                |> PG.toCmd jwt (tagger << Result.mapError PGError)

        Nothing ->
            fail <| BadSchema resourcesName


fail : Error -> Cmd Msg
fail msg =
    Task.fail msg |> Task.attempt Failure

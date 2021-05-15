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

import Basics.Extra exposing (flip)
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
        , href
        , id
        , list
        , target
        )
import Html.Events exposing (onInput)
import Inflect as String
import Maybe.Extra as Maybe
import Postgrest.Client as PG exposing (PostgrestErrorJSON)
import Postgrest.Field as Field exposing (Field)
import Postgrest.PrimaryKey as PrimaryKey
import Postgrest.Resource as Resource exposing (Resource)
import Postgrest.Resource.Client as Client exposing (Client)
import Postgrest.Value as Value exposing (ForeignKeyParams, Value(..))
import Result
import String.Extra as String
import Url.Builder as Url


type alias Record =
    Dict String Input


type alias Autocomplete =
    { results : List Resource
    , userInput : String
    , blocked : Bool
    , foreignKeyParams : ForeignKeyParams
    }


type Msg
    = Changed String Input String
    | AutocompleteInput String Field Autocomplete String
    | ListingFetched String Field Autocomplete AutocompleteResult
    | Failure (Result Error Never)


type alias AutocompleteResult =
    Result Error (List Resource)


type Input
    = Text Field
    | Number Field
    | Checkbox Field
    | DateTime Field
    | Association Field Autocomplete
    | Blank Field


update : Client a -> Msg -> Record -> ( Record, Cmd Msg )
update client msg record =
    case msg of
        Changed name input userInput ->
            let
                value =
                    toField input
                        |> .value
                        |> Value.updateWithString userInput
            in
            ( Dict.insert name (updateInput value input) record, Cmd.none )

        AutocompleteInput name field autocomplete "" ->
            let
                association =
                    Association field
                        { autocomplete | userInput = "", blocked = False }
            in
            ( Dict.insert name association record, Cmd.none )

        AutocompleteInput name field ({ foreignKeyParams } as ac) userInput ->
            let
                mresource =
                    findResource ac userInput

                mprimaryKey =
                    Maybe.andThen Resource.primaryKey mresource

                label =
                    Maybe.andThen (resourceLabel foreignKeyParams) mresource

                foreignKeyParams_ =
                    { foreignKeyParams | label = label }

                value =
                    PForeignKey mprimaryKey foreignKeyParams_

                field_ =
                    Field.update value field

                association =
                    Association field_ autocomplete

                autocomplete =
                    { ac
                        | userInput = Maybe.withDefault userInput label
                        , foreignKeyParams = foreignKeyParams_
                        , blocked = False
                        , results = []
                    }
            in
            ( Dict.insert name association record
            , fetchResources client name field_ autocomplete
            )

        ListingFetched name field autocomplete result ->
            case result of
                Ok [] ->
                    let
                        association =
                            Association field
                                { autocomplete | results = [], blocked = True }
                    in
                    ( Dict.insert name association record
                    , Cmd.none
                    )

                Ok results ->
                    let
                        autocomplete_ =
                            { autocomplete | results = results }
                    in
                    ( Dict.insert name (Association field autocomplete_) record
                    , Cmd.none
                    )

                Err _ ->
                    ( record, Cmd.none )

        Failure _ ->
            ( record, Cmd.none )


updateInput : Value -> Input -> Input
updateInput value input =
    case input of
        Text field ->
            Text <| Field.update value field

        Number field ->
            Number <| Field.update value field

        Checkbox field ->
            Checkbox <| Field.update value field

        DateTime field ->
            DateTime <| Field.update value field

        Association field autocomplete ->
            Association (Field.update value field) autocomplete

        _ ->
            input


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

        PForeignKey _ params ->
            Association field
                { userInput = params.label |> Maybe.withDefault ""
                , results = []
                , blocked = False
                , foreignKeyParams = params
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


displayAutocompleteInput : Autocomplete -> Field -> String -> Html Msg
displayAutocompleteInput ({ foreignKeyParams } as autocomplete) field name =
    let
        makeDatalist =
            Html.datalist [ id foreignKeyParams.table ]

        datalist =
            if Value.isNothing field.value then
                List.map (autocompleteOption foreignKeyParams)
                    autocomplete.results
                    |> makeDatalist

            else
                makeDatalist []

        tagger =
            AutocompleteInput name field autocomplete

        prevLength =
            String.length autocomplete.userInput

        inputCallback string =
            if
                autocomplete.blocked
                    && (String.length string >= prevLength)
            then
                tagger autocomplete.userInput

            else
                tagger string
    in
    div []
        [ datalist
        , Html.input
            [ onInput inputCallback
            , id name
            , list foreignKeyParams.table
            , if field.required then
                attribute "aria-required" "true"

              else
                class ""
            , Html.Attributes.type_ "text"
            , Html.Attributes.value autocomplete.userInput
            ]
            []
        , associationLink field
        ]


associationLink : Field -> Html Msg
associationLink { value } =
    case value of
        PForeignKey (Just primaryKey) { table, label } ->
            let
                id =
                    PrimaryKey.toString primaryKey
            in
            a
                [ href <| Url.absolute [ table, id ] []
                , target "_blank"
                ]
                [ Maybe.map ((++) (id ++ " - ")) label
                    |> Maybe.withDefault id
                    |> text
                ]

        _ ->
            text ""


autocompleteOption : ForeignKeyParams -> Resource -> Html Msg
autocompleteOption params resource =
    option [ Html.Attributes.value <| optionText params resource ] []


optionText : ForeignKeyParams -> Resource -> String
optionText ({ primaryKeyName } as params) resource =
    let
        id =
            Resource.fieldToString primaryKeyName resource
    in
    [ id, resourceLabel params resource ]
        |> List.filterMap identity
        |> String.join " - "


resourceLabel : ForeignKeyParams -> Resource -> Maybe String
resourceLabel { labelColumnName } resource =
    Maybe.andThen (flip Resource.fieldToString resource) labelColumnName


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


fetchResources : Client a -> String -> Field -> Autocomplete -> Cmd Msg
fetchResources client name field ({ foreignKeyParams } as autocomplete) =
    case Dict.get foreignKeyParams.table client.schema of
        Just definition ->
            let
                selects =
                    foreignKeyParams.labelColumnName
                        |> Maybe.map (flip (::) [ "id" ])
                        |> Maybe.withDefault [ "id" ]
                        |> PG.attributes

                idQuery =
                    String.toInt autocomplete.userInput
                        |> Maybe.map (PG.param "id" << PG.eq << PG.int)

                ilike column input =
                    PG.param column <| PG.ilike ("*" ++ input ++ "*")

                labelQuery =
                    Maybe.map2 ilike
                        foreignKeyParams.labelColumnName
                        (String.nonBlank autocomplete.userInput)

                queries =
                    List.filterMap identity [ idQuery, labelQuery ]
            in
            if List.isEmpty queries then
                Error.fail Failure <|
                    AutocompleteError foreignKeyParams autocomplete.userInput

            else
                Client.fetchMany client definition foreignKeyParams.table
                    |> PG.setParams
                        [ PG.select selects, PG.or queries, PG.limit 40 ]
                    |> PG.toCmd client.jwt
                        (ListingFetched name field autocomplete
                            << Result.mapError PGError
                        )

        Nothing ->
            Error.fail Failure <| BadSchema foreignKeyParams.table


findResource : Autocomplete -> String -> Maybe Resource
findResource ({ foreignKeyParams } as autocomplete) userInput =
    let
        userInputLower =
            String.toLower userInput

        findByOptionText resource =
            String.toLower (optionText foreignKeyParams resource)
                == userInputLower

        findByLabel resource =
            case resourceLabel foreignKeyParams resource of
                Just lab ->
                    String.toLower lab == userInputLower

                Nothing ->
                    False
    in
    List.filter findByOptionText autocomplete.results
        ++ List.filter findByLabel autocomplete.results
        |> List.head


updateAssociation : String -> Field -> Autocomplete -> ( Field, Input )
updateAssociation name field ({ foreignKeyParams, userInput } as ac) =
    let
        mresource =
            findResource ac userInput

        mprimaryKey =
            Maybe.andThen Resource.primaryKey mresource

        label =
            Maybe.andThen (resourceLabel foreignKeyParams) mresource

        foreignKeyParams_ =
            { foreignKeyParams | label = label }

        value =
            PForeignKey mprimaryKey foreignKeyParams_

        field_ =
            Field.update value field

        autocomplete_ =
            { ac
                | userInput = Maybe.withDefault userInput label
                , foreignKeyParams = foreignKeyParams_
                , blocked = False
            }
    in
    ( field_, Association field_ autocomplete_ )

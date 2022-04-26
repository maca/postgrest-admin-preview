module Form.Input exposing
    ( Input(..)
    , Msg
    , fromField
    , isRequired
    , mapMsg
    , setError
    , toField
    , toValue
    , update
    , view
    )

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Html exposing (Html, a, div, label, option, p, text)
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
        , required
        , selected
        , target
        )
import Html.Events exposing (onInput)
import Maybe.Extra as Maybe
import Postgrest.Client as PG exposing (PostgrestErrorJSON)
import Postgrest.Constraint exposing (Constraint(..))
import Postgrest.Field as Field exposing (Field)
import Postgrest.PrimaryKey as PrimaryKey
import Postgrest.Resource as Resource exposing (Resource)
import Postgrest.Resource.Client as Client exposing (Client)
import Postgrest.Value as Value exposing (ForeignKeyParams, Value(..))
import PostgrestAdmin.AuthScheme as AuthScheme
import PostgrestAdmin.OuterMsg as OuterMsg exposing (OuterMsg)
import Result
import String.Extra as String
import Url.Builder as Url
import Utils.Task exposing (Error(..), fail)


type alias Fields =
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
    | Failed Error


type alias AutocompleteResult =
    Result Error (List Resource)


type Input
    = Text Field
    | TextArea Field
    | Select Field (List String)
    | Number Field
    | Checkbox Field
    | DateTime Field
    | Date Field
    | Association Field Autocomplete
    | Blank Field


mapMsg : Msg -> OuterMsg
mapMsg msg =
    case msg of
        Failed err ->
            OuterMsg.RequestFailed err

        _ ->
            OuterMsg.Pass


update : Client a -> Msg -> Fields -> ( Fields, Cmd Msg )
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
                value =
                    PForeignKey Nothing autocomplete.foreignKeyParams

                association =
                    Association (Field.update value field)
                        { autocomplete | userInput = "", blocked = False }
            in
            ( Dict.insert name association record, Cmd.none )

        AutocompleteInput name field autocomplete userInput ->
            case field.constraint of
                ForeignKey prevParams ->
                    let
                        label =
                            findResource autocomplete userInput
                                |> Maybe.andThen
                                    (resourceLabel autocomplete.foreignKeyParams)

                        params =
                            { prevParams | label = label }

                        field_ =
                            { field | constraint = ForeignKey params }

                        association =
                            Association field_ autocomplete
                    in
                    ( Dict.insert name association record
                    , fetchResources client name field_ <|
                        { autocomplete
                            | userInput = Maybe.withDefault userInput label
                            , foreignKeyParams = params
                            , blocked = False
                            , results = []
                        }
                    )

                _ ->
                    ( record, Cmd.none )

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

        Failed _ ->
            ( record, Cmd.none )


updateInput : Value -> Input -> Input
updateInput value input =
    case input of
        Text field ->
            Text <| Field.update value field

        TextArea field ->
            TextArea <| Field.update value field

        Select field opts ->
            Select (Field.update value field) opts

        Number field ->
            Number <| Field.update value field

        Checkbox field ->
            Checkbox <| Field.update value field

        DateTime field ->
            DateTime <| Field.update value field

        Date field ->
            Date <| Field.update value field

        Association field autocomplete ->
            Association (Field.update value field) autocomplete

        Blank _ ->
            input


toField : Input -> Field
toField input =
    case input of
        Text field ->
            field

        TextArea field ->
            field

        Select field _ ->
            field

        Number field ->
            field

        Checkbox field ->
            field

        DateTime field ->
            field

        Date field ->
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

        PText _ ->
            TextArea field

        PEnum _ opts ->
            Select field opts

        PFloat _ ->
            Number field

        PInt _ ->
            Number field

        PBool _ ->
            Checkbox field

        PTime _ ->
            DateTime field

        PDate _ ->
            Date field

        PForeignKey _ params ->
            Association field
                { userInput = params.label |> Maybe.withDefault ""
                , results = []
                , blocked = False
                , foreignKeyParams = params
                }

        PPrimaryKey _ ->
            Blank field

        Unknown _ ->
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

        TextArea { value } ->
            Value.toString value
                |> displayTextArea input
                |> wrapInput input name

        Select { value } opts ->
            Value.toString value
                |> displaySelect opts input
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

        Date { value } ->
            Value.toString value
                |> displayInput "date" input
                |> wrapInput input name

        Association field params ->
            displayAutocompleteInput params field |> wrapInput input name

        Blank _ ->
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
    div [ class "autocomplete-input" ]
        [ datalist
        , div [ class "association-link" ] [ associationLink field ]
        , Html.input
            [ onInput inputCallback
            , id name
            , list foreignKeyParams.table
            , required field.required
            , Html.Attributes.type_ "text"
            , Html.Attributes.value autocomplete.userInput
            ]
            []
        ]


associationLink : Field -> Html Msg
associationLink { value } =
    case value of
        PForeignKey (Just primaryKey) { table } ->
            let
                id =
                    PrimaryKey.toString primaryKey
            in
            a
                [ href <| Url.absolute [ table, id ] [], target "_blank" ]
                [ text id ]

        _ ->
            text "-"


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
        , required <| isRequired input
        , Html.Attributes.type_ type_
        , Html.Attributes.value <| Maybe.withDefault "" mstring
        ]
        []


displayTextArea : Input -> Maybe String -> String -> Html Msg
displayTextArea input mstring name =
    let
        content =
            Maybe.withDefault "" mstring
    in
    div
        [ class "grow-wrap"
        , attribute "data-replicated-value" content
        ]
        [ Html.textarea
            [ onInput <| Changed name input
            , id name
            , required <| isRequired input
            , Html.Attributes.value content
            ]
            []
        ]


displaySelect : List String -> Input -> Maybe String -> String -> Html Msg
displaySelect options input mstring name =
    let
        optionValues =
            if isRequired input then
                options

            else
                "" :: options

        selectOption opt =
            Html.option
                [ Html.Attributes.value opt, selected <| mstring == Just opt ]
                [ text <| String.humanize opt ]
    in
    Html.select
        [ onInput <| Changed name input
        , id name
        , required <| isRequired input
        , Html.Attributes.value <| Maybe.withDefault "" mstring
        ]
        (List.map selectOption optionValues)


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
        Just table ->
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
                fail Failed <|
                    AutocompleteError foreignKeyParams autocomplete.userInput

            else
                case AuthScheme.toJwt client.authScheme of
                    Just token ->
                        Client.fetchMany client table foreignKeyParams.table
                            |> PG.setParams
                                [ PG.select selects
                                , PG.or queries
                                , PG.limit 40
                                ]
                            |> PG.toCmd token
                                (ListingFetched name field autocomplete
                                    << Result.mapError PGError
                                )

                    Nothing ->
                        fail Failed AuthError

        Nothing ->
            fail Failed <| BadSchema foreignKeyParams.table


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

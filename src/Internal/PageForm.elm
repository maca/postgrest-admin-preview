module Internal.PageForm exposing
    ( Msg
    , PageForm
    , errors
    , init
    , onLogin
    , toId
    , update
    , view
    )

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (Html, button, fieldset, h2, section, text)
import Html.Attributes exposing (autocomplete, class, disabled, novalidate)
import Html.Events exposing (onSubmit)
import Internal.Cmd as AppCmd
import Internal.Field as Field
import Internal.Http exposing (Error(..))
import Internal.Input as Input exposing (Input)
import Internal.Schema exposing (Constraint(..), Table)
import Internal.Value exposing (Value(..))
import Internal.ViewHelp exposing (breadcrumbs)
import PostgRestAdmin.Client as Client exposing (Client)
import PostgRestAdmin.Notification as Notification
import PostgRestAdmin.Record as Record exposing (Record)
import Url
import Url.Builder as Url


type Msg
    = LoggedIn Client
    | Fetched (Result Error Record)
    | Saved (Result Error Record)
    | InputChanged Input.Msg
    | Submitted


type alias Inputs =
    Dict String Input


type PageForm
    = PageForm
        { client : Client
        , key : Nav.Key
        , inputs : Inputs
        , fieldNames : List String
        , table : Table
        , id : Maybe String
        , parent : Maybe { tableName : String, id : String }
        }


init :
    { client : Client
    , fieldNames : List String
    , id : Maybe String
    , table : Table
    , parent : Maybe { tableName : String, id : String }
    }
    -> Nav.Key
    -> ( PageForm, AppCmd.Cmd Msg )
init { client, fieldNames, id, table, parent } key =
    let
        pageForm =
            PageForm
                { client = client
                , key = key
                , inputs =
                    Maybe.map (always Dict.empty) id
                        |> Maybe.withDefault
                            (recordToInputs (Record.fromTable table))
                , fieldNames = fieldNames
                , table = table
                , id = id
                , parent = parent
                }
    in
    ( pageForm, fetchRecord pageForm )


fetchRecord : PageForm -> AppCmd.Cmd Msg
fetchRecord (PageForm { id, client, table }) =
    case id of
        Just recordId ->
            Client.fetchRecord
                { client = client
                , table = table
                , id = recordId
                , expect = Fetched
                }

        Nothing ->
            AppCmd.none


onLogin : Client -> Msg
onLogin =
    LoggedIn



-- Update


update : Msg -> PageForm -> ( PageForm, AppCmd.Cmd Msg )
update msg (PageForm params) =
    case msg of
        LoggedIn client ->
            let
                pageForm =
                    PageForm { params | client = client }
            in
            ( pageForm
            , if params.inputs == Dict.empty then
                fetchRecord pageForm

              else
                AppCmd.none
            )

        Fetched (Ok record) ->
            ( PageForm { params | inputs = recordToInputs record }
            , AppCmd.none
            )

        Fetched (Err _) ->
            ( PageForm params, AppCmd.none )

        Saved (Ok record) ->
            ( PageForm params
            , AppCmd.batch
                [ Url.absolute
                    [ params.table.name
                    , Record.id record |> Maybe.withDefault ""
                    ]
                    []
                    |> Nav.pushUrl params.key
                    |> AppCmd.wrap
                , Notification.confirm "The record was saved"
                ]
            )

        Saved (Err _) ->
            ( PageForm params, AppCmd.none )

        InputChanged inputMsg ->
            let
                ( inputs, cmd ) =
                    Input.update params.client inputMsg params.inputs
            in
            ( PageForm { params | inputs = inputs }
            , AppCmd.batch [ AppCmd.map InputChanged cmd, Notification.dismiss ]
            )

        Submitted ->
            ( PageForm params
            , Client.saveRecord
                { client = params.client
                , record = toRecord (PageForm params)
                , id = params.id
                , expect = Saved
                }
            )



-- UTILS


toRecord : PageForm -> Record
toRecord (PageForm { table, inputs, parent, id }) =
    { table = table
    , fields =
        Dict.map
            (\_ input ->
                let
                    field =
                        Input.toField input
                in
                case field.constraint of
                    ForeignKey params ->
                        case parent of
                            Just p ->
                                if params.tableName == p.tableName then
                                    Field.updateWithString p.id field

                                else
                                    field

                            _ ->
                                field

                    _ ->
                        field
            )
            inputs
    , persisted = Maybe.map (always True) id |> Maybe.withDefault False
    }


formInputs : PageForm -> List ( String, Input )
formInputs (PageForm { inputs }) =
    Dict.toList inputs |> List.sortWith sortInputs


filterInputs : PageForm -> PageForm
filterInputs (PageForm params) =
    PageForm
        { params
            | inputs =
                Dict.filter (inputIsEditable params.fieldNames) params.inputs
        }


inputIsEditable : List String -> String -> Input -> Bool
inputIsEditable fieldNames name input =
    let
        field =
            Input.toField input
    in
    (List.isEmpty fieldNames && not (Field.isPrimaryKey field))
        || List.member name fieldNames


recordToInputs : Record -> Inputs
recordToInputs record =
    record.fields |> Dict.map (\_ input -> Input.fromField input)


changed : PageForm -> Bool
changed (PageForm { inputs }) =
    Dict.values inputs |> List.any (.changed << Input.toField)


errors : PageForm -> Dict String (Maybe String)
errors record =
    toRecord record |> Record.errors


hasErrors : PageForm -> Bool
hasErrors record =
    toRecord record |> Record.hasErrors


toId : PageForm -> Maybe String
toId (PageForm params) =
    params.id



-- VIEW


view : PageForm -> Html Msg
view ((PageForm { table, id, parent }) as form) =
    let
        fields =
            filterInputs form
                |> formInputs
                |> List.map
                    (\( name, input ) ->
                        let
                            inputView =
                                Input.toHtml name input
                                    |> Html.map InputChanged
                        in
                        case Input.toField input |> .constraint of
                            ForeignKey params ->
                                case parent of
                                    Just { tableName } ->
                                        if params.tableName == tableName then
                                            text ""

                                        else
                                            inputView

                                    Nothing ->
                                        inputView

                            _ ->
                                inputView
                    )
    in
    section
        [ class "resource-form" ]
        [ breadcrumbs table.name [ table.name, Maybe.withDefault "new" id ]
        , h2
            []
            [ Record.label (toRecord form)
                |> Maybe.map text
                |> Maybe.withDefault (text "")
            ]
        , Html.form
            [ autocomplete False
            , onSubmit Submitted
            , novalidate True
            ]
            [ fieldset [] fields
            , fieldset []
                [ button
                    [ disabled (not (changed form) || hasErrors form) ]
                    [ text "Save" ]
                ]
            ]
        ]



-- Sort


sortInputs : ( String, Input ) -> ( String, Input ) -> Order
sortInputs ( name, input ) ( name_, input_ ) =
    Field.compareTuple
        ( name, Input.toField input )
        ( name_, Input.toField input_ )

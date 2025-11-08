module Internal.PageForm exposing
    ( Msg
    , PageForm
    , init
    , onLogin
    , update
    , view
    )

import Browser.Navigation as Nav
import Dict exposing (Dict)
import FormToolkit.Field as Field2
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Http
import Internal.Cmd as AppCmd
import Internal.Field as Field
import Internal.Http exposing (Error)
import Internal.Input as Input exposing (Input)
import Internal.Schema exposing (ColumnType(..), Constraint(..), Table)
import Internal.Value as Value exposing (Value(..))
import Json.Decode as Decode
import PostgRestAdmin.Client as Client exposing (Client)
import PostgRestAdmin.MountPath exposing (MountPath, breadcrumbs, path)
import PostgRestAdmin.Notification as Notification
import PostgRestAdmin.Record as Record exposing (Record)
import String.Extra as String
import Url.Builder as Url


type Msg
    = LoggedIn Client
    | Fetched (Result Http.Error Decode.Value)
    | ParentFetched (Result Http.Error Decode.Value)
    | Saved (Result Error Client.Response)
    | InputChanged Input.Msg
    | FormChanged (Field2.Msg ())
    | Submitted


type alias Inputs =
    Dict String Input


type alias PageForm =
    { client : Client
    , mountPath : MountPath
    , key : Nav.Key
    , inputs : Inputs
    , fieldNames : List String
    , table : Table
    , id : Maybe String
    , parent : Maybe Record
    , form : Field2.Field ()
    , persisted : Bool
    }


init :
    { client : Client
    , mountPath : MountPath
    , fieldNames : List String
    , id : Maybe String
    , table : Table
    , parent : Maybe { tableName : String, id : String }
    }
    -> Nav.Key
    -> ( PageForm, AppCmd.Cmd Msg )
init { client, mountPath, fieldNames, id, table, parent } key =
    let
        parentParams =
            Maybe.andThen
                (\params ->
                    Client.getTable params.tableName client
                        |> Maybe.map (\parentTable -> ( parentTable, params ))
                )
                parent

        pageForm =
            { client = client
            , mountPath = mountPath
            , key = key
            , inputs =
                Maybe.map (always Dict.empty) id
                    |> Maybe.withDefault
                        (recordToInputs (Record.fromTable table))
            , fieldNames = fieldNames
            , table = table
            , id = id
            , parent = Nothing
            , form = buildForm table
            , persisted = False
            }
    in
    ( pageForm
    , AppCmd.batch
        [ fetchRecord pageForm
        , case parentParams of
            Just ( parentTable, params ) ->
                Client.fetchRecord2
                    { client = client
                    , table = parentTable
                    , id = params.id
                    , expect = ParentFetched
                    }

            Nothing ->
                AppCmd.none
        ]
    )


fetchRecord : PageForm -> AppCmd.Cmd Msg
fetchRecord { id, client, table } =
    case id of
        Just recordId ->
            Client.fetchRecord2
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
update msg model =
    case msg of
        LoggedIn client ->
            ( { model | client = client }
            , if Dict.isEmpty model.inputs then
                fetchRecord { model | client = client }

              else
                AppCmd.none
            )

        Fetched (Ok record) ->
            ( model
            , AppCmd.none
            )

        Fetched (Err err) ->
            ( model
            , Debug.todo "crash"
              -- , Notification.error (Internal.Http.errorToString err)
            )

        ParentFetched (Ok parent) ->
            -- ( { model | parent = Just parent }, AppCmd.none )
            ( model, AppCmd.none )

        ParentFetched (Err err) ->
            ( model
            , Debug.todo "crash"
              -- , Notification.error (Http.errorToString err)
            )

        Saved (Ok record) ->
            ( model
            , AppCmd.none
              -- , AppCmd.batch
              --     [ Url.absolute
              --         [ model.table.name
              --         , Record.id record |> Maybe.withDefault ""
              --         ]
              --         []
              --         |> Nav.pushUrl model.key
              --         |> AppCmd.wrap
              --     , Notification.confirm "The record was saved"
              --     ]
            )

        Saved (Err err) ->
            ( model
            , Debug.todo "crash"
              -- , Notification.error (Http.errorToString err)
            )

        InputChanged inputMsg ->
            let
                ( inputs, cmd ) =
                    Input.update model.client inputMsg model.inputs
            in
            ( { model | inputs = inputs }
            , AppCmd.batch [ AppCmd.map InputChanged cmd, Notification.dismiss ]
            )

        FormChanged innerMsg ->
            ( { model | form = Field2.update innerMsg model.form }
            , Notification.dismiss
            )

        Submitted ->
            ( model
            , AppCmd.none
              -- , Client.saveRecord
              --     { client = model.client
              --     , record = toRecord model
              --     , id = model.id
              --     , expect = Saved
              --     }
            )



-- UTILS


toRecord : PageForm -> Record
toRecord { table, inputs, parent, id } =
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
                            Just record ->
                                if
                                    params.tableName
                                        == .name (Record.getTable record)
                                then
                                    Record.id record
                                        |> Maybe.map
                                            (\s -> Field.updateWithString s field)
                                        |> Maybe.withDefault field

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
formInputs { inputs } =
    Dict.toList inputs |> List.sortWith sortInputs


filterInputs : PageForm -> PageForm
filterInputs params =
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
    record.fields
        |> Dict.map
            (\name field ->
                case Dict.get name record.table.columns of
                    Just column ->
                        Input.fromFieldAndColumn field column

                    Nothing ->
                        Input.fromFieldAndColumn field
                            { constraint = NoConstraint
                            , required = False
                            , value = PString Nothing
                            , columnType = String
                            , options = []
                            }
            )


changed : PageForm -> Bool
changed { inputs } =
    Dict.values inputs |> List.any (.changed << Input.toField)


hasErrors : PageForm -> Bool
hasErrors record =
    toRecord record |> Record.hasErrors



-- VIEW


view : PageForm -> Html Msg
view ({ table, id, parent, mountPath } as model) =
    let
        fields =
            filterInputs model
                |> formInputs
                |> List.map
                    (\( name, input ) ->
                        let
                            inputView =
                                Input.toHtml mountPath name input
                                    |> Html.map InputChanged
                        in
                        case Input.toField input |> .constraint of
                            ForeignKey params ->
                                case parent of
                                    Just record ->
                                        if
                                            params.tableName
                                                == .name (Record.getTable record)
                                        then
                                            Html.text ""

                                        else
                                            inputView

                                    Nothing ->
                                        inputView

                            _ ->
                                inputView
                    )
    in
    Html.section
        [ Attrs.class "resource-form" ]
        [ breadcrumbs mountPath
            table.name
            (case id of
                Just idStr ->
                    [ ( table.name, Nothing )
                    , ( idStr, Record.label (toRecord model) )
                    , ( "edit", Nothing )
                    ]

                Nothing ->
                    case parent of
                        Just record ->
                            [ ( Record.getTable record |> .name, Nothing )
                            , ( Record.id record |> Maybe.withDefault ""
                              , Record.label record
                              )
                            , ( table.name, Nothing )
                            , ( "new", Nothing )
                            ]

                        Nothing ->
                            [ ( table.name, Nothing )
                            , ( "new", Nothing )
                            ]
            )
        , Html.h2
            []
            [ Record.label (toRecord model)
                |> Maybe.map Html.text
                |> Maybe.withDefault (Html.text "")
            ]
        , Html.form
            [ Attrs.autocomplete False
            , Events.onSubmit Submitted
            , Attrs.novalidate True
            ]
            [ Html.fieldset [] fields
            , Field2.toHtml FormChanged model.form
            , Html.fieldset []
                [ Html.a
                    [ Attrs.class "button button-clear"
                    , Attrs.href
                        (path mountPath <|
                            Url.absolute
                                (List.filterMap identity
                                    [ Maybe.map (Record.getTable >> .name) parent
                                    , Maybe.andThen Record.id parent
                                    , Just table.name
                                    , id
                                    ]
                                )
                                []
                        )
                    ]
                    [ Html.text "Cancel" ]
                , Html.button
                    [ Attrs.disabled (not (changed model) || hasErrors model) ]
                    [ Html.text "Save" ]
                ]
            ]
        ]


buildForm : Table -> Field2.Field ()
buildForm table =
    Field2.group []
        (Dict.toList table.columns
            |> List.sortWith sortColumns
            |> List.map (\( name, column ) -> fieldFromColumn name column)
        )


fieldFromColumn : String -> Internal.Schema.Column -> Field2.Field ()
fieldFromColumn name column =
    let
        baseAttrs =
            List.filterMap identity
                [ Just (Field2.name name)
                , Just (Field2.label (String.humanize name))
                , Just (Field2.required column.required)
                , Maybe.map Field2.stringValue (Value.toString column.value)
                ]

        stringOptions =
            column.options
                |> List.filterMap Value.toString
    in
    case column.constraint of
        PrimaryKey ->
            Field2.text (baseAttrs ++ [ Field2.disabled True ])

        ForeignKey _ ->
            -- For now, treat foreign keys as text inputs
            -- TODO: Implement autocomplete with association
            Field2.text baseAttrs

        NoConstraint ->
            case column.columnType of
                Internal.Schema.String ->
                    if not (List.isEmpty stringOptions) then
                        Field2.select (baseAttrs ++ [ Field2.stringOptions stringOptions ])

                    else
                        Field2.text baseAttrs

                Internal.Schema.Text ->
                    Field2.textarea (baseAttrs ++ [ Field2.autogrow True ])

                Internal.Schema.Float ->
                    Field2.float baseAttrs

                Internal.Schema.Integer ->
                    Field2.int baseAttrs

                Internal.Schema.Boolean ->
                    Field2.checkbox baseAttrs

                Internal.Schema.TimestampWithoutTimezome ->
                    Field2.datetime baseAttrs

                Internal.Schema.Timestamp ->
                    Field2.datetime baseAttrs

                Internal.Schema.TimeWithoutTimezone ->
                    Field2.datetime baseAttrs

                Internal.Schema.Time ->
                    Field2.datetime baseAttrs

                Internal.Schema.Date ->
                    Field2.date baseAttrs

                Internal.Schema.Json ->
                    Field2.textarea (baseAttrs ++ [ Field2.autogrow True ])

                Internal.Schema.Uuid ->
                    Field2.text baseAttrs

                _ ->
                    Field2.text (baseAttrs ++ [ Field2.disabled True ])


sortColumns : ( String, Internal.Schema.Column ) -> ( String, Internal.Schema.Column ) -> Order
sortColumns ( name, column ) ( name_, column_ ) =
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
            compare name name_



-- Sort


sortInputs : ( String, Input ) -> ( String, Input ) -> Order
sortInputs ( name, input ) ( name_, input_ ) =
    Field.compareTuple
        ( name, Input.toField input )
        ( name_, Input.toField input_ )

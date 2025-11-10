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
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Http
import Internal.Cmd as AppCmd
import Internal.Input as Input exposing (Input)
import Internal.Schema as Schema exposing (ColumnType(..), Constraint(..), Table)
import Internal.Value as Value exposing (Value(..))
import Json.Decode as Decode
import PostgRestAdmin.Client as Client exposing (Client)
import PostgRestAdmin.MountPath as MountPath exposing (MountPath, path)
import PostgRestAdmin.Notification as Notification
import PostgRestAdmin.Record as Record exposing (Record)
import String.Extra as String
import Url.Builder as Url


type Msg
    = LoggedIn Client
    | Fetched (Result Http.Error Decode.Value)
    | ParentFetched (Result Http.Error Table)
    | Saved (Result Http.Error ( Decode.Value, Table ))
    | FormChanged (Field.Msg String)
    | Submitted


type alias Inputs =
    Dict String Input


type alias PageForm =
    { client : Client
    , mountPath : MountPath
    , key : Nav.Key
    , inputs : Inputs
    , table : Table
    , id : Maybe String
    , parent : Maybe Table
    , form : Field.Field String
    , persisted : Bool
    }


init :
    { client : Client
    , navKey : Nav.Key
    , mountPath : MountPath
    , id : Maybe String
    , table : Table
    , parent : Maybe { tableName : String, id : String }
    }
    -> ( PageForm, AppCmd.Cmd Msg )
init { client, navKey, mountPath, id, table, parent } =
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
            , key = navKey
            , inputs =
                Maybe.map (always Dict.empty) id
                    |> Maybe.withDefault
                        (recordToInputs (Record.fromTable table))
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
                    , decoder = Schema.tableUpdateDecoder table
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
                , decoder = Decode.value
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

        Fetched (Ok response) ->
            ( { model
                | form =
                    Field.updateValuesFromJson response model.form
                        |> Result.withDefault model.form
              }
            , AppCmd.none
            )

        Fetched (Err err) ->
            ( model
            , Notification.error (Client.httpErrorToString err)
            )

        ParentFetched (Ok parent) ->
            ( { model | parent = Just parent }, AppCmd.none )

        ParentFetched (Err err) ->
            ( model
            , Notification.error (Client.httpErrorToString err)
            )

        Saved (Ok ( response, table )) ->
            let
                id =
                    Schema.tablePrimaryKeyValue table
                        |> Maybe.andThen (Tuple.second >> Value.toString)

                url =
                    Url.absolute [ model.table.name, Maybe.withDefault "" id ] []

                form =
                    Field.updateValuesFromJson response model.form
                        |> Result.withDefault model.form
            in
            ( { model | id = id, form = form, table = table }
            , AppCmd.batch
                [ AppCmd.wrap (Nav.pushUrl model.key url)
                , Notification.confirm "The record was saved"
                ]
            )

        Saved (Err err) ->
            ( model
            , Notification.error (Client.httpErrorToString err)
            )

        FormChanged innerMsg ->
            ( { model | form = Field.update innerMsg model.form }
            , Notification.dismiss
            )

        Submitted ->
            ( { model | form = Field.validate model.form |> Field.touch }
            , case Parse.parse Parse.json model.form of
                Ok jsonBody ->
                    Client.saveRecord
                        { client = model.client
                        , id = model.id
                        , body = jsonBody
                        , table = model.table
                        , expect = Saved
                        , decoder =
                            Decode.map2 Tuple.pair
                                Decode.value
                                (Schema.tableUpdateDecoder model.table)
                        }

                Err _ ->
                    Notification.error "Please check the form errors"
            )


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



-- VIEW


view : PageForm -> Html Msg
view ({ table, id, parent, mountPath } as model) =
    Html.section
        [ Attrs.class "resource-form" ]
        [ MountPath.breadcrumbs mountPath
            table.name
            (case id of
                Just idStr ->
                    [ ( table.name, Nothing )
                    , ( idStr, Schema.label model.table )
                    , ( "edit", Nothing )
                    ]

                Nothing ->
                    case parent of
                        Just parentTable ->
                            [ ( parentTable.name, Nothing )
                            , ( Schema.tablePrimaryKeyValue parentTable
                                    |> Maybe.andThen (Tuple.second >> Value.toString)
                                    |> Maybe.withDefault ""
                              , Schema.label parentTable
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
            [ Schema.label model.table
                |> Maybe.map Html.text
                |> Maybe.withDefault (Html.text "")
            ]
        , let
            formValid =
                Parse.parse (Parse.succeed True) model.form
                    |> Result.withDefault False
          in
          Html.form
            [ Attrs.autocomplete False
            , if formValid then
                Events.onSubmit Submitted

              else
                Attrs.class ""
            , Attrs.novalidate True
            ]
            [ Field.toHtml FormChanged model.form
            , Html.fieldset []
                [ Html.a
                    [ Attrs.class "button button-clear"
                    , Attrs.href
                        (path mountPath <|
                            Url.absolute
                                (List.filterMap identity
                                    [ Maybe.map .name parent
                                    , Maybe.andThen
                                        (Schema.tablePrimaryKeyValue
                                            >> Maybe.andThen (Tuple.second >> Value.toString)
                                        )
                                        parent
                                    , Just table.name
                                    , id
                                    ]
                                )
                                []
                        )
                    ]
                    [ Html.text "Cancel" ]
                , Html.button
                    [ Attrs.type_ "submit"
                    , Attrs.disabled (not formValid)
                    ]
                    [ Html.text "Save" ]
                ]
            ]
        ]


buildForm : Table -> Field.Field String
buildForm table =
    Field.group []
        (Dict.toList table.columns
            |> List.sortWith sortColumns
            |> List.filterMap
                (\( name, column ) ->
                    fieldFromColumn name column
                )
        )


fieldFromColumn : String -> Schema.Column -> Maybe (Field.Field String)
fieldFromColumn name column =
    let
        attrs =
            List.concatMap identity
                [ [ Field.name name
                  , Field.identifier name
                  , Field.label (String.humanize name)
                  , Field.required column.required
                  ]
                , Value.toString column.value
                    |> Maybe.map (Field.stringValue >> List.singleton)
                    |> Maybe.withDefault []
                , if column.constraint == PrimaryKey then
                    [ Field.disabled True
                    , Field.required False
                    ]

                  else
                    []
                ]
    in
    case column.constraint of
        ForeignKey _ ->
            -- For now, treat foreign keys as text inputs
            -- TODO: Implement autocomplete with association
            Just (Field.text attrs)

        _ ->
            Just
                (case column.columnType of
                    Schema.String ->
                        case column.options |> List.filterMap Value.toString of
                            [] ->
                                Field.text attrs

                            options ->
                                Field.select (Field.stringOptions options :: attrs)

                    Schema.Text ->
                        Field.textarea (Field.autogrow True :: attrs)

                    Schema.Float ->
                        Field.float attrs

                    Schema.Integer ->
                        Field.int attrs

                    Schema.Boolean ->
                        Field.checkbox attrs

                    Schema.TimestampWithoutTimezome ->
                        Field.datetime attrs

                    Schema.Timestamp ->
                        Field.datetime attrs

                    Schema.TimeWithoutTimezone ->
                        Field.datetime attrs

                    Schema.Time ->
                        Field.datetime attrs

                    Schema.Date ->
                        Field.date attrs

                    Schema.Json ->
                        Field.textarea (Field.autogrow True :: attrs)

                    Schema.Uuid ->
                        Field.text attrs

                    _ ->
                        Field.text (Field.disabled True :: attrs)
                )


sortColumns : ( String, Schema.Column ) -> ( String, Schema.Column ) -> Order
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

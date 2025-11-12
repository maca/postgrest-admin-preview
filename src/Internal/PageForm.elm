module Internal.PageForm exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Dict exposing (Dict)
import FormToolkit.Error
import FormToolkit.Field as Field
import FormToolkit.Parse as Parse
import FormToolkit.Value as FormValue
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Http
import Internal.Cmd as AppCmd
import Internal.Schema as Schema exposing (Constraint(..), Table)
import Internal.Value as Value
import Json.Decode as Decode
import PostgRestAdmin.Client as Client exposing (Client)
import PostgRestAdmin.MountPath as MountPath exposing (MountPath, path)
import PostgRestAdmin.Notification as Notification
import Postgrest.Client as PG
import String.Extra as String
import Task
import Url.Builder as Url


type Msg
    = Fetched (Result Client.Error Decode.Value)
    | ParentLabelFetched (Result Client.Error String)
    | AutocompleteValuesFetched String (Result Client.Error AutocompleteOptions)
    | Saved (Result Client.Error String)
    | FormChanged (Field.Msg String)
    | Submitted


type alias Model =
    { client : Client
    , table : Table
    , mountPath : MountPath
    , key : Nav.Key
    , id : Maybe String
    , parent : Maybe { tableName : String, id : String }
    , parentLabel : Maybe String
    , form : Field.Field String
    , autocompleteValues : Dict String AutocompleteOptions
    }


type AutocompleteOptions
    = Local (List { id : String, label : String })
    | Remote


init :
    { client : Client
    , navKey : Nav.Key
    , mountPath : MountPath
    , table : Table
    , id : Maybe String
    , parent : Maybe { tableName : String, id : String }
    }
    -> ( Model, AppCmd.Cmd Msg )
init { client, navKey, mountPath, table, id, parent } =
    ( { client = client
      , table = table
      , mountPath = mountPath
      , key = navKey
      , id = id
      , parent = parent
      , parentLabel = Nothing
      , form = buildForm table
      , autocompleteValues = Dict.empty
      }
    , AppCmd.batch
        (List.concat
            [ [ Maybe.map2 Tuple.pair (Schema.tablePrimaryKeyName table) id
                    |> Maybe.map (fetch client table)
                    |> Maybe.withDefault AppCmd.none
              , parent
                    |> Maybe.andThen (Schema.buildParentReference client.schema table)
                    |> Maybe.map (fetchParentLabel client)
                    |> Maybe.withDefault AppCmd.none
              ]
            , Schema.buildReferences table
                |> Dict.toList
                |> List.map (fetchAutcompleteValues client)
            ]
        )
    )


fetchAutcompleteValues : Client -> ( String, Schema.ForeignKeyParams ) -> AppCmd.Cmd Msg
fetchAutcompleteValues client ( colName, ref ) =
    Client.count
        { client = client
        , path = Url.absolute [ ref.tableName ] []
        }
        |> Task.andThen
            (\count ->
                case ( count < 1000, ref.labelColumnName ) of
                    ( True, Just labelColumnName ) ->
                        Client.task
                            { client = client
                            , method = "GET"
                            , headers = []
                            , path =
                                String.concat
                                    [ "/"
                                    , ref.tableName
                                    , "?"
                                    , PG.toQueryString
                                        [ PG.select
                                            (PG.attributes
                                                [ ref.primaryKeyName
                                                , labelColumnName
                                                ]
                                            )
                                        ]
                                    ]
                            , body = Http.emptyBody
                            , decoder =
                                Decode.list
                                    (Decode.oneOf
                                        [ Decode.map2
                                            (\id label -> Just { id = id, label = label })
                                            (Decode.field ref.primaryKeyName Decode.string)
                                            (Decode.field labelColumnName Decode.string)
                                        , Decode.succeed Nothing
                                        ]
                                    )
                                    |> Decode.map (List.filterMap identity >> Local)
                            }

                    _ ->
                        Task.succeed Remote
            )
        |> Task.attempt (AutocompleteValuesFetched colName)
        |> AppCmd.wrap


fetch : Client -> Table -> ( String, String ) -> AppCmd.Cmd Msg
fetch client table ( primaryKeyName, recordId ) =
    let
        selectedCols =
            PG.select
                (Dict.keys table.columns
                    |> List.filter ((/=) primaryKeyName)
                    |> List.map PG.attribute
                )

        queryString =
            PG.toQueryString
                [ selectedCols
                , PG.param primaryKeyName (PG.eq (PG.string recordId))
                , PG.limit 1
                ]
    in
    Client.task
        { client = client
        , method = "GET"
        , headers = [ Http.header "Accept" "application/vnd.pgrst.object+json" ]
        , path = "/" ++ table.name ++ "?" ++ queryString
        , body = Http.emptyBody
        , decoder = Decode.value
        }
        |> Task.attempt Fetched
        |> AppCmd.wrap


fetchParentLabel :
    Client
    ->
        { parentLabelColumn : String
        , parentPrimaryKey : String
        , parentId : String
        , parentTable : Table
        }
    -> AppCmd.Cmd Msg
fetchParentLabel client params =
    let
        selectedCols =
            PG.select
                [ PG.attribute params.parentLabelColumn ]

        queryString =
            PG.toQueryString
                [ selectedCols
                , PG.param params.parentPrimaryKey (PG.eq (PG.string params.parentId))
                , PG.limit 1
                ]
    in
    Client.task
        { client = client
        , method = "GET"
        , headers = [ Http.header "Accept" "application/vnd.pgrst.object+json" ]
        , path = "/" ++ params.parentTable.name ++ "?" ++ queryString
        , body = Http.emptyBody
        , decoder = Decode.field params.parentLabelColumn Decode.string
        }
        |> Task.attempt ParentLabelFetched
        |> AppCmd.wrap



-- Update


update : Msg -> Model -> ( Model, AppCmd.Cmd Msg )
update msg model =
    case msg of
        Fetched (Ok response) ->
            case Field.updateValuesFromJson response model.form of
                Ok form ->
                    ( { model | form = form }
                    , AppCmd.none
                    )

                Err err ->
                    ( model
                    , Notification.error (FormToolkit.Error.toEnglish err)
                    )

        Fetched (Err err) ->
            ( model
            , Notification.error (Client.errorToString err)
            )

        ParentLabelFetched (Ok label) ->
            ( { model | parentLabel = Just label, form = updateFormParent model label }
            , AppCmd.none
            )

        ParentLabelFetched (Err err) ->
            ( model
            , Notification.error (Client.errorToString err)
            )

        AutocompleteValuesFetched colName (Ok values) ->
            ( { model
                | autocompleteValues =
                    Dict.insert colName values model.autocompleteValues
                , form = updateAutocompletOptions colName values model.form
              }
            , AppCmd.none
            )

        AutocompleteValuesFetched _ (Err err) ->
            ( model
            , Notification.error (Client.errorToString err)
            )

        Saved (Ok id) ->
            ( model
            , AppCmd.batch
                [ AppCmd.wrap
                    (Nav.pushUrl model.key
                        (Url.absolute [ model.table.name, id ] [])
                    )
                , Notification.confirm "The record was saved"
                ]
            )

        Saved (Err err) ->
            ( model
            , Notification.error (Client.errorToString err)
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
                            case Schema.tablePrimaryKey model.table of
                                Just ( pkName, column ) ->
                                    Decode.field pkName
                                        (Schema.valueDecoder column.columnType)
                                        |> Decode.map
                                            (Value.toString >> Maybe.withDefault "")

                                Nothing ->
                                    Decode.fail "Coudn't figure the field id"
                        }

                Err _ ->
                    Notification.error "Please check the form errors"
            )


updateFormParent : Model -> String -> Field.Field String
updateFormParent model label =
    model.parent
        |> Maybe.map
            (\p ->
                case
                    Dict.filter (\_ v -> p.tableName == v.tableName)
                        (Schema.buildReferences model.table)
                        |> Dict.keys
                of
                    [ colName ] ->
                        (case Dict.get colName model.autocompleteValues of
                            Just (Local _) ->
                                model.form

                            _ ->
                                Field.updateWithId colName
                                    (Field.options [ autocompleteOption { id = p.id, label = label } ])
                                    model.form
                        )
                            |> Field.updateWithId colName
                                (Field.stringValue (autocompleteLabel { id = p.id, label = label }))

                    _ ->
                        model.form
            )
        |> Maybe.withDefault model.form


updateAutocompletOptions : String -> AutocompleteOptions -> Field.Field String -> Field.Field String
updateAutocompletOptions colName values =
    case values of
        Local list ->
            Field.updateWithId colName
                (Field.options (List.map autocompleteOption list))

        Remote ->
            identity


autocompleteOption : { id : String, label : String } -> ( String, FormValue.Value )
autocompleteOption attrs =
    ( autocompleteLabel attrs, FormValue.string attrs.id )


autocompleteLabel : { id : String, label : String } -> String
autocompleteLabel { label, id } =
    label ++ " (" ++ id ++ ")"



-- VIEW


view : Model -> Html Msg
view ({ table, id, mountPath } as model) =
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
                    case model.parent of
                        Just parent ->
                            [ ( parent.tableName, Nothing )
                            , ( parent.id, model.parentLabel )
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
                                    [ Maybe.map .tableName model.parent
                                    , Maybe.map .id model.parent
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
            |> List.filterMap fieldFromColumn
        )


fieldFromColumn : ( String, Schema.Column ) -> Maybe (Field.Field String)
fieldFromColumn ( name, column ) =
    let
        attrs =
            List.concat
                [ [ Field.name name
                  , Field.identifier name
                  , Field.label (String.humanize name)
                  , Field.required column.required
                  ]
                , Value.toString column.value
                    |> Maybe.map (Field.stringValue >> List.singleton)
                    |> Maybe.withDefault []
                ]
    in
    case column.constraint of
        ForeignKey _ ->
            Just (Field.strictAutocomplete attrs)

        PrimaryKey ->
            Nothing

        NoConstraint ->
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

module Internal.PageListing exposing
    ( Model
    , Msg
    , ascendingBy
    , descendingBy
    , hideSearch
    , init
    , onLogin
    , subscriptions
    , update
    , view
    )

import Browser.Dom as Dom exposing (Viewport)
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Bytes exposing (Bytes)
import Csv exposing (Csv)
import Dict
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (Html)
import Html.Attributes
    exposing
        ( attribute
        , class
        , classList
        , disabled
        , href
        , id
        )
import Html.Events as Events
    exposing
        ( onClick
        )
import Http
import Inflect
import Internal.Cmd as AppCmd
import Internal.Schema as Schema
    exposing
        ( Column
        , ColumnType(..)
        , Constraint(..)
        , Record
        , Table
        , Value(..)
        )
import Internal.Search as Search exposing (Search)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import List.Split as List
import Markdown
import PostgRestAdmin.Client as Client exposing (Client, Count, Error, errorToString)
import PostgRestAdmin.MountPath as MountPath exposing (MountPath)
import PostgRestAdmin.Notification as Notification
import PostgRestAdmin.Views as Views
import Postgrest.Client as PG
import Set
import String.Extra as String
import Task
import Time
import Url
import Url.Builder as Url exposing (QueryParameter)


type Page
    = Page (List Record)
    | Blank


type SortOrder
    = Asc String
    | Desc String
    | Unordered


type alias CsvUpload =
    { missingColumns : List String
    , extraColumns : List String
    , headers : List String
    , records : List ( Int, Record )
    }


type UploadState
    = Idle
    | Fetching
    | BadCsvSchema CsvUpload
    | UploadReady (List ( Int, Record ))
    | UploadWithErrors (List ( Int, Record ))


type Format
    = CSV
    | JSON


type Msg
    = LoggedIn Client
    | Fetched (Result Error ( List Record, Count ))
    | ParentLabelFetched (Result Client.Error String)
    | ApplyFilters
    | Sort SortOrder
    | Reload
    | Scrolled
    | ScrollInfo (Result Dom.Error Viewport)
    | SearchChanged Search.Msg
    | DownloadRequested Format
    | Downloaded Format (Result Error Bytes)
    | ToggleSearchOpen
    | CsvUploadRequested
    | CsvUploadSelected File
    | CsvUploadLoaded String
    | CsvProcessed Csv (Result Error (List Record))
    | CsvUploadPosted (Result Error ())
    | CsvUploadAccepted
    | CsvUploadCanceled
    | NoOp


type alias Model =
    { client : Client
    , mountPath : MountPath
    , key : Nav.Key
    , table : Table
    , columns : List ( String, Column )
    , parent : Maybe { tableName : String, id : String }
    , parentLabel : Maybe String
    , foreignKey : Maybe ( String, String )
    , scrollPosition : Float
    , pages : List Page
    , page : Int
    , order : SortOrder
    , search : Search
    , searchOpen : Bool
    , uploadState : UploadState
    }


init :
    { client : Client
    , mountPath : MountPath
    , table : Table
    , parent : Maybe { tableName : String, id : String }
    }
    -> Url.Url
    -> Nav.Key
    -> ( Model, AppCmd.Cmd Msg )
init { client, mountPath, table, parent } url key =
    let
        order =
            Maybe.map parseQuery url.query
                |> Maybe.withDefault []
                |> List.filter (Tuple.first >> (==) "order")
                |> List.head
                |> Maybe.map (Tuple.second >> parseOrder)
                |> Maybe.withDefault Unordered

        model =
            { client = client
            , mountPath = mountPath
            , key = key
            , table = table
            , columns =
                Schema.tableToSortedColumnList table
                    |> List.filter isColumnVisible
            , parent = parent
            , parentLabel = Nothing
            , foreignKey = parentReference table parent
            , page = 0
            , scrollPosition = 0
            , pages = []
            , order = order
            , search = Search.init table (url.query |> Maybe.withDefault "")
            , searchOpen = False
            , uploadState = Idle
            }
    in
    ( model
    , AppCmd.batch
        [ Client.fetchRecords
            { client = client
            , path =
                listingPath
                    { limit = True
                    , selectAll = False
                    , nest = False
                    }
                    model
            , table = table
            }
            |> Task.attempt Fetched
            |> AppCmd.wrap
        , parent
            |> Maybe.andThen (Schema.buildParentReference client.schema table)
            |> Maybe.map
                (Client.fetchParentLabel client
                    >> Task.attempt ParentLabelFetched
                    >> AppCmd.wrap
                )
            |> Maybe.withDefault AppCmd.none
        ]
    )


onLogin : Client -> Msg
onLogin =
    LoggedIn


isSearchVisible : Model -> Bool
isSearchVisible { searchOpen, search } =
    searchOpen || Search.isBlank search


hideSearch : Model -> Model
hideSearch listing =
    { listing | searchOpen = False }


ascendingBy : String -> Model -> Model
ascendingBy column listing =
    { listing | order = Asc column }


descendingBy : String -> Model -> Model
descendingBy column listing =
    { listing | order = Desc column }


fetch : Model -> AppCmd.Cmd Msg
fetch model =
    Client.fetchRecords
        { client = model.client
        , path =
            listingPath
                { limit = True
                , selectAll = False
                , nest = False
                }
                model
        , table = model.table
        }
        |> Task.attempt Fetched
        |> AppCmd.wrap


update : Msg -> Model -> ( Model, AppCmd.Cmd Msg )
update msg listing =
    case msg of
        LoggedIn client ->
            fetchListing
                { listing | client = client }

        Fetched (Ok ( records, count )) ->
            let
                recordCount =
                    List.length records
            in
            ( { listing
                | page = listing.page + 1
                , pages =
                    case listing.pages of
                        Blank :: pages ->
                            if recordCount < perPage then
                                Blank :: Page records :: pages

                            else
                                Page records :: pages

                        pages ->
                            let
                                loadedRecords =
                                    (List.length pages * perPage) + recordCount
                            in
                            if loadedRecords >= count.total then
                                Blank :: Page records :: pages

                            else
                                Page records :: pages
              }
            , AppCmd.none
            )

        Fetched (Err err) ->
            case listing.order of
                Unordered ->
                    ( listing
                    , Notification.error (errorToString err)
                    )

                _ ->
                    ( { listing | order = Unordered }
                    , reload listing.table
                    )

        ParentLabelFetched (Ok parentLabel) ->
            ( { listing | parentLabel = Just parentLabel }
            , AppCmd.none
            )

        ParentLabelFetched (Err err) ->
            ( listing, Notification.error (Client.errorToString err) )

        ApplyFilters ->
            ( listing, reload listing.table )

        Sort order ->
            ( { listing | order = order }, reload listing.table )

        Reload ->
            ( listing
            , listing
                |> listingPath { limit = False, selectAll = True, nest = True }
                |> MountPath.path listing.mountPath
                |> Nav.replaceUrl listing.key
                |> AppCmd.wrap
            )

        Scrolled ->
            ( listing
            , Dom.getViewportOf listing.table.name
                |> Task.attempt ScrollInfo
                |> AppCmd.wrap
            )

        ScrollInfo result ->
            case result of
                Ok viewport ->
                    if
                        scrollingDown viewport listing
                            && closeToBottom viewport
                    then
                        case listing.pages of
                            Blank :: _ ->
                                ( { listing
                                    | scrollPosition = viewport.viewport.y
                                  }
                                , AppCmd.none
                                )

                            _ ->
                                fetchListing
                                    { listing
                                        | scrollPosition = viewport.viewport.y
                                        , pages = Blank :: listing.pages
                                    }

                    else
                        ( { listing | scrollPosition = viewport.viewport.y }
                        , AppCmd.none
                        )

                Err _ ->
                    ( listing
                    , Notification.error "Dom element not found"
                    )

        SearchChanged searchMsg ->
            Search.update searchMsg listing.search
                |> Tuple.mapFirst (\search -> { listing | search = search })
                |> Tuple.mapSecond (searchChanged searchMsg)

        ToggleSearchOpen ->
            ( { listing | searchOpen = not listing.searchOpen }
            , AppCmd.none
            )

        DownloadRequested format ->
            let
                path =
                    listingPath
                        { limit = False
                        , selectAll = True
                        , nest = False
                        }
                        listing
            in
            ( listing
            , Client.bytesRequest
                { client = listing.client
                , method = "GET"
                , headers =
                    [ case format of
                        CSV ->
                            Http.header "Accept" "text/csv"

                        JSON ->
                            Http.header "Accept" "application/json"
                    ]
                , path = path
                , body = Http.emptyBody
                }
                |> Task.attempt (Downloaded format)
                |> AppCmd.wrap
            )

        Downloaded format (Ok body) ->
            let
                download ext =
                    (listing.table.name ++ "-list." ++ ext)
                        |> Download.bytes
            in
            ( listing
            , AppCmd.wrap
                (case format of
                    CSV ->
                        download "csv" "text/csv" body

                    JSON ->
                        download "json" "application/json" body
                )
            )

        Downloaded _ (Err err) ->
            ( listing, Notification.error (Client.errorToString err) )

        CsvUploadRequested ->
            ( listing
            , AppCmd.batch
                [ Task.attempt (always NoOp) (Dom.blur "upload-csv-button")
                    |> AppCmd.wrap
                , Select.file [ "text/csv" ] CsvUploadSelected
                    |> AppCmd.wrap
                ]
            )

        CsvUploadSelected file ->
            ( listing
            , Task.perform CsvUploadLoaded (File.toString file)
                |> AppCmd.wrap
            )

        CsvUploadLoaded string ->
            case Csv.parse string of
                Ok rawCsv ->
                    let
                        csv =
                            { rawCsv
                                | headers = List.map String.trim rawCsv.headers
                            }

                        headers =
                            Set.fromList csv.headers

                        columnNames =
                            Set.fromList (Dict.keys listing.table.columns)

                        missing =
                            Set.diff columnNames headers

                        extra =
                            Set.diff headers columnNames
                    in
                    if Set.isEmpty extra && Set.isEmpty missing then
                        ( { listing | uploadState = Fetching }
                        , processCsv listing csv
                        )

                    else
                        ( { listing
                            | uploadState =
                                BadCsvSchema
                                    { extraColumns = Set.toList extra
                                    , missingColumns = Set.toList missing
                                    , headers = csv.headers
                                    , records =
                                        buildImportedRecords listing [] csv
                                    }
                          }
                        , AppCmd.none
                        )

                Err _ ->
                    ( listing
                    , Notification.error "This CSV file cound not be parsed."
                    )

        CsvProcessed csv (Ok existing) ->
            let
                records =
                    buildImportedRecords listing existing csv
            in
            case List.filter (\( _, rec ) -> hasErrors rec) records of
                [] ->
                    ( { listing | uploadState = UploadReady records }
                    , AppCmd.none
                    )

                withErrors ->
                    ( { listing | uploadState = UploadWithErrors withErrors }
                    , AppCmd.none
                    )

        CsvProcessed _ (Err err) ->
            ( listing
            , Notification.error (errorToString err)
            )

        CsvUploadPosted (Ok ()) ->
            ( listing
            , AppCmd.batch
                [ Dom.setViewportOf listing.table.name 0 0
                    |> Task.attempt (always Reload)
                    |> AppCmd.wrap
                , Notification.confirm "The upload was succesful."
                ]
            )

        CsvUploadPosted (Err err) ->
            ( listing, Notification.error (Client.errorToString err) )

        CsvUploadAccepted ->
            ( listing
            , case listing.uploadState of
                UploadReady records ->
                    Client.request
                        { client = listing.client
                        , method = "POST"
                        , headers =
                            [ Http.header "Prefer"
                                "resolution=merge-duplicates"
                            ]
                        , path = Url.absolute [ listing.table.name ] []
                        , body =
                            Http.jsonBody
                                (Encode.list
                                    (\( _, rec ) -> Debug.todo "crash")
                                    records
                                )
                        , decoder = Decode.succeed ()
                        , expect = CsvUploadPosted
                        }

                _ ->
                    AppCmd.none
            )

        CsvUploadCanceled ->
            ( { listing | uploadState = Idle }, AppCmd.none )

        NoOp ->
            ( listing, AppCmd.none )


reload : Table -> AppCmd.Cmd Msg
reload table =
    Dom.setViewportOf table.name 0 0
        |> Task.attempt (always Reload)
        |> AppCmd.wrap


scrollingDown : Viewport -> Model -> Bool
scrollingDown { viewport } { scrollPosition } =
    scrollPosition < viewport.y


closeToBottom : Viewport -> Bool
closeToBottom { scene, viewport } =
    scene.height - viewport.y < (viewport.height * 2)


fetchListing : Model -> ( Model, AppCmd.Cmd Msg )
fetchListing listing =
    ( listing, fetch listing )


searchChanged : Search.Msg -> Cmd Search.Msg -> AppCmd.Cmd Msg
searchChanged msg cmd =
    if Search.isApplyMsg msg then
        Time.now
            |> Task.perform (always ApplyFilters)
            |> AppCmd.wrap

    else
        Cmd.map SearchChanged cmd |> AppCmd.wrap


listingPath :
    { limit : Bool, selectAll : Bool, nest : Bool }
    -> Model
    -> String
listingPath { limit, selectAll, nest } ({ parent } as model) =
    let
        selectQuery =
            if selectAll then
                []

            else
                [ PG.select
                    (List.concat
                        [ model.columns
                            |> List.map (Tuple.first >> PG.attribute)
                        , model.columns
                            |> List.filterMap
                                (Tuple.second >> Client.associationJoin)
                        ]
                    )
                ]

        parentQuery =
            model.foreignKey
                |> Maybe.map
                    (\( colName, id ) ->
                        [ PG.param colName (PG.eq (PG.string id)) ]
                    )
                |> Maybe.withDefault []

        limitQuery =
            if limit then
                [ PG.limit perPage, PG.offset (perPage * model.page) ]

            else
                []

        queryParams =
            orderToQueryParams model.order

        baseUrl =
            if nest then
                Url.absolute
                    (List.filterMap identity
                        [ -- Maybe.map (Record.getTable >> .name) parent
                          -- , Maybe.andThen Record.id parent
                          Just model.table.name
                        ]
                    )
                    (orderToQueryParams model.order)

            else
                Url.absolute [ model.table.name ]
                    (orderToQueryParams model.order)

        filterQuery =
            PG.toQueryString <|
                parentQuery
                    ++ limitQuery
                    ++ selectQuery
                    ++ Search.toPGQuery model.search

        joinChar =
            if List.isEmpty queryParams then
                "?"

            else
                "&"
    in
    [ baseUrl, filterQuery ]
        |> List.filterMap String.nonBlank
        |> String.join joinChar



-- UPLOAD


buildImportedRecords : Model -> List Record -> Csv -> List ( Int, Record )
buildImportedRecords { table, foreignKey } persistedRecords csv =
    let
        primaryKeyName =
            Schema.tablePrimaryKeyName table

        persisted =
            persistedRecords
                |> List.map
                    (\rec ->
                        ( primaryKey table rec
                            |> Maybe.andThen
                                (\{ value } -> valueToString value)
                            |> Maybe.withDefault ""
                        , rec
                        )
                    )
                |> Dict.fromList
    in
    csv.records
        |> List.indexedMap
            (\idx row ->
                let
                    blank =
                        Dict.fromList (List.zip csv.headers row)

                    dict =
                        foreignKey
                            |> Maybe.map
                                (\( colName, fkVal ) ->
                                    Dict.get colName blank
                                        |> Maybe.andThen String.nonBlank
                                        |> Maybe.map (always blank)
                                        |> Maybe.withDefault
                                            (Dict.insert colName fkVal blank)
                                )
                            |> Maybe.withDefault blank

                    record =
                        primaryKeyName
                            |> Maybe.andThen (\pkName -> Dict.get pkName dict)
                            |> Maybe.andThen (\id -> Dict.get id persisted)
                            |> Maybe.withDefault Dict.empty
                            |> setAssociationValidation foreignKey
                in
                ( idx + 1
                , Dict.foldl updateWithString record dict
                )
            )


setAssociationValidation : Maybe ( String, String ) -> Record -> Record
setAssociationValidation foreignKey record =
    record


processCsv : Model -> Csv -> AppCmd.Cmd Msg
processCsv { table, client } csv =
    let
        primaryKeyName =
            Schema.tablePrimaryKeyName table

        ids =
            primaryKeyName
                |> Maybe.map
                    (\pkName ->
                        let
                            blank =
                                Dict.empty
                        in
                        List.filterMap
                            (List.zip csv.headers
                                >> Dict.fromList
                                >> Dict.get pkName
                                >> Maybe.andThen
                                    (\id ->
                                        updateWithString pkName id blank
                                            |> primaryKey table
                                    )
                                >> Maybe.andThen
                                    (\{ value } -> valueToString value)
                            )
                            csv.records
                            |> List.chunksOfLeft 100
                    )
                |> Maybe.withDefault []
    in
    if List.isEmpty ids then
        CsvProcessed csv (Ok [])
            |> Task.succeed
            |> Task.perform identity
            |> AppCmd.wrap

    else
        ids
            |> List.map (Client.chunk client table primaryKeyName)
            |> Task.sequence
            |> Task.map List.concat
            |> Task.attempt (CsvProcessed csv)
            |> AppCmd.wrap



-- VIEW


view : Model -> Html Msg
view model =
    Html.section
        [ class "resources-listing" ]
        [ viewPageHeader model
        , Html.div
            [ id model.table.name
            , class "resources-listing-results"
            , case model.pages of
                Blank :: _ ->
                    class ""

                _ ->
                    Events.on "scroll" (Decode.succeed Scrolled)
            ]
            [ uploadModal model
            , viewRecordsTable model
            ]
        , Html.aside
            [ class "listing-controls" ]
            [ Html.div
                [ class "controls" ]
                [ Html.div
                    [ class "downloads" ]
                    [ Html.button
                        [ class "button-clear"
                        , onClick (DownloadRequested JSON)
                        ]
                        [ Html.text "Download JSON" ]
                    , Html.button
                        [ class "button-clear"
                        , onClick (DownloadRequested CSV)
                        ]
                        [ Html.text "Download CSV" ]
                    ]
                , Html.div
                    []
                    [ if Search.isBlank model.search then
                        Html.text ""

                      else
                        toggleSearchButton model
                    , Html.button
                        [ onClick ApplyFilters
                        , disabled (Search.isBlank model.search)
                        ]
                        [ Html.text "Apply Filters" ]
                    ]
                ]
            , Html.map SearchChanged
                (Search.view (isSearchVisible model) model.search)
            ]
        ]


viewPageHeader : Model -> Html Msg
viewPageHeader ({ mountPath, table } as model) =
    Html.header
        []
        [ MountPath.breadcrumbs mountPath
            table.name
            (case model.parent of
                Just parent ->
                    [ ( parent.tableName, Nothing )
                    , ( parent.id, model.parentLabel )
                    , ( table.name, Nothing )
                    ]

                Nothing ->
                    [ ( table.name, Nothing ) ]
            )
        , Html.div
            []
            [ Html.button
                [ id "upload-csv-button"
                , class "button"
                , class ("button-upload-csv-" ++ table.name)
                , onClick CsvUploadRequested
                ]
                [ Html.text "Upload CSV" ]
            , Html.a
                [ class "button"
                , class ("button-new-" ++ table.name)
                , href
                    (MountPath.path mountPath <|
                        Url.absolute
                            (List.filterMap identity
                                [ Maybe.map .tableName model.parent
                                , Maybe.map .id model.parent
                                , Just table.name
                                , Just "new"
                                ]
                            )
                            []
                    )
                ]
                [ Html.text
                    ("New " ++ (String.humanize table.name |> Inflect.toSingular))
                ]
            ]
        ]


viewRecordsTable : Model -> Html Msg
viewRecordsTable model =
    Html.table []
        (Html.thead []
            [ Html.tr [] (List.map (viewHeaderCell model) model.columns ++ [ Html.th [] [] ]) ]
            :: (List.reverse model.pages
                    |> List.indexedMap
                        (\pageNum page ->
                            case page of
                                Page records ->
                                    Html.tbody
                                        [ id (pageId pageNum) ]
                                        (List.map
                                            (\record ->
                                                Html.tr
                                                    [ class "listing-row" ]
                                                    (viewRowCells model record
                                                        ++ [ viewActions model record ]
                                                    )
                                            )
                                            records
                                        )

                                Blank ->
                                    Html.text ""
                        )
               )
        )


viewHeaderCell : Model -> ( String, Column ) -> Html Msg
viewHeaderCell { order } ( name, _ ) =
    let
        defaultHeader =
            Html.span
                [ class "sort"
                , attribute "aria-sort" "other"
                , onClick <| Sort <| Asc name
                ]
                [ Html.text <| String.humanize name
                , Html.i [ class "icono-play" ] []
                ]
    in
    Html.th
        []
        [ case order of
            Asc col ->
                if col == name then
                    Html.span
                        [ class "sort"
                        , attribute "aria-sort" "ascending"
                        , onClick <| Sort <| Desc name
                        ]
                        [ Html.text <| String.humanize name
                        , Html.i [ class "asc icono-play" ] []
                        ]

                else
                    defaultHeader

            Desc col ->
                if col == name then
                    Html.span
                        [ class "sort"
                        , attribute "aria-sort" "descending"
                        , onClick <| Sort <| Asc name
                        ]
                        [ Html.text <| String.humanize name
                        , Html.i [ class "desc icono-play" ] []
                        ]

                else
                    defaultHeader

            Unordered ->
                defaultHeader
        ]


viewRowCells : Model -> Record -> List (Html Msg)
viewRowCells model record =
    List.filterMap
        (\( colName, col ) ->
            Dict.get colName record
                |> Maybe.map
                    (\value ->
                        Html.td
                            []
                            [ Html.span
                                []
                                [ Views.renderValue model.mountPath col value
                                ]
                            ]
                    )
        )
        model.columns


viewActions : Model -> Record -> Html Msg
viewActions model record =
    Html.td []
        [ recordId model.table record
            |> Maybe.map
                (\id ->
                    Html.a
                        [ class "button button-clear button-small"
                        , href
                            (MountPath.path model.mountPath <|
                                Url.absolute
                                    [ recordTableName model.table, id ]
                                    []
                            )
                        ]
                        [ Html.text "View" ]
                )
            |> Maybe.withDefault (Html.text "")
        ]


toggleSearchButton : Model -> Html Msg
toggleSearchButton model =
    Html.button
        [ class "toggle-button"
        , class "button-clear"
        , classList [ ( "open", isSearchVisible model ) ]
        , onClick ToggleSearchOpen
        ]
        [ Html.i [ class "icono-play" ] []
        , if isSearchVisible model then
            Html.text "Hide"

          else
            Html.text "Show"
        , Html.text " Filters"
        ]


uploadModal : Model -> Html Msg
uploadModal model =
    let
        fieldNames =
            List.map Tuple.first model.columns
    in
    case model.uploadState of
        UploadReady records ->
            Html.div
                [ class "modal-background"
                , class "upload-preview"
                ]
                [ uploadPreview (Debug.todo "crash") fieldNames records ]

        UploadWithErrors records ->
            Html.div
                [ class "modal-background"
                , class "upload-preview"
                ]
                [ uploadWithErrorsPreview (Debug.todo "crash") fieldNames records
                ]

        BadCsvSchema csvUpload ->
            Html.div
                [ class "modal-background"
                , class "upload-preview"
                ]
                [ badSchemaPreview csvUpload ]

        Idle ->
            Html.text ""

        Fetching ->
            Html.text ""


uploadPreview : Maybe Record -> List String -> List ( Int, Record ) -> Html Msg
uploadPreview parent fieldNames records =
    let
        ( toUpdate, toCreate ) =
            -- TODO: persisted field no longer exists in simplified Record type
            ( [], records )
    in
    Html.div
        [ class "modal-dialog" ]
        [ Html.h2
            []
            [ Html.text "CSV Upload" ]
        , case parent of
            Just record ->
                Html.p
                    []
                    [ Html.text "Upload records for "

                    -- , text <| String.humanize (.name (Record.getTable record))
                    -- , Record.id record
                    --     |> Maybe.map (\id -> text (" with id of " ++ id ++ "."))
                    --     |> Maybe.withDefault (text "")
                    ]

            Nothing ->
                Html.text ""
        , Html.div
            [ class "csv-records-preview" ]
            [ previewUploadTable "Records to create" fieldNames toCreate
            , previewUploadTable "Records to update" fieldNames toUpdate
            ]
        , Markdown.toHtml [ class "disclaimer" ] <|
            "Please review the following changes: "
                ++ (List.filterMap identity
                        [ previewUploadCopy "created" toCreate
                        , previewUploadCopy "updated" toUpdate
                        ]
                        |> String.toSentence
                   )
                ++ ".\n This action cannot be undone."
        , Html.div
            [ class "actions" ]
            [ Html.button
                [ class "button"
                , class "button-clear"
                , onClick CsvUploadCanceled
                ]
                [ Html.text "Cancel" ]
            , Html.button
                [ class "button button"
                , onClick CsvUploadAccepted
                ]
                [ Html.text "Save" ]
            ]
        ]


previewUploadCopy : String -> List ( Int, Record ) -> Maybe String
previewUploadCopy action records =
    if List.isEmpty records then
        Nothing

    else
        Just
            ("**"
                ++ (List.length records |> String.fromInt)
                ++ String.surround " "
                    (case records of
                        _ :: [] ->
                            "record"

                        _ ->
                            "records"
                    )
                ++ "** will be "
                ++ action
            )


previewUploadTable : String -> List String -> List ( Int, Record ) -> Html Msg
previewUploadTable action fieldNames records =
    if List.isEmpty records then
        Html.text ""

    else
        Html.div
            []
            [ Html.h4
                []
                [ Html.text action
                , Html.b []
                    [ Html.text " ("
                    , Html.text (List.length records |> String.fromInt)
                    , Html.text ")"
                    ]
                ]
            , previewTable fieldNames records
            ]


badSchemaPreview : CsvUpload -> Html Msg
badSchemaPreview { extraColumns, missingColumns, headers, records } =
    let
        displayColumns =
            List.map (String.surround "**") >> String.toSentence
    in
    Html.div
        [ class "modal-dialog" ]
        [ Html.h2 [] [ Html.text "Wrong columns" ]
        , Html.p []
            [ ("The CSV file "
                ++ (List.filterMap identity
                        [ if not (List.isEmpty missingColumns) then
                            Just
                                ("is missing the following columns: "
                                    ++ displayColumns missingColumns
                                )

                          else
                            Nothing
                        , if not (List.isEmpty extraColumns) then
                            Just
                                ("should not contain the following columns: "
                                    ++ displayColumns extraColumns
                                )

                          else
                            Nothing
                        ]
                        |> String.join ", and it"
                   )
                ++ "."
              )
                |> Markdown.toHtml []
            , Html.p
                []
                [ Html.text "Please update the CSV and try again." ]
            ]
        , Html.div
            [ class "csv-records-preview" ]
            [ previewTable headers records ]
        , Html.div
            [ class "actions" ]
            [ Html.button
                [ class "button button"
                , onClick CsvUploadCanceled
                ]
                [ Html.text "Ok" ]
            ]
        ]


uploadWithErrorsPreview :
    Maybe Record
    -> List String
    -> List ( Int, Record )
    -> Html Msg
uploadWithErrorsPreview parent fieldNames records =
    Html.div
        [ class "modal-dialog" ]
        [ Html.h2
            []
            [ Html.text "Validation failed" ]
        , Html.p []
            [ case parent of
                Just record ->
                    Html.text <|
                        "There where some errors validating the records. "

                -- ++ String.humanize (.name (Record.getTable record))
                -- ++ (Record.id record
                --         |> Maybe.map
                --             (\id -> " with id of " ++ id ++ ". ")
                --         |> Maybe.withDefault ""
                --    )
                Nothing ->
                    Html.text "There where some errors validating the records. "
            , Html.br [] []
            , Html.text "Please update the CSV and try again."
            , Html.br [] []
            ]
        , Html.div
            [ class "csv-records-preview" ]
            [ previewTable fieldNames
                (List.filter (\( _, rec ) -> hasErrors rec) records)
            ]
        , Html.div
            [ class "actions" ]
            [ Html.button
                [ class "button button"
                , onClick CsvUploadCanceled
                ]
                [ Html.text "Ok" ]
            ]
        ]


previewTable : List String -> List ( Int, Record ) -> Html Msg
previewTable fieldNames records =
    Html.table
        []
        [ Html.thead
            []
            (List.map
                (\name -> Html.th [] [ Html.text name ])
                ("Row No." :: fieldNames)
            )
        , Html.tbody
            []
            (List.map
                (\( idx, record ) ->
                    Html.tr
                        [ if hasErrors record then
                            class "with-errors"

                          else
                            class ""
                        ]
                        (Html.td [] [ Html.text (String.fromInt idx) ]
                            :: List.map (previewListCell record) fieldNames
                        )
                )
                records
            )
        ]


previewListCell : Record -> String -> Html Msg
previewListCell record fieldName =
    Html.td
        []
        (Dict.get fieldName record
            |> Maybe.map
                (\value ->
                    [ Html.span [] [ fieldValueToHtml value ] ]
                )
            |> Maybe.withDefault [ Html.text "" ]
        )


pageId : Int -> String
pageId pageNum =
    "page-" ++ String.fromInt pageNum


perPage : Int
perPage =
    50


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown
        (Decode.andThen
            (\key ->
                case key of
                    27 ->
                        Decode.succeed CsvUploadCanceled

                    13 ->
                        case model.uploadState of
                            BadCsvSchema _ ->
                                Decode.succeed CsvUploadCanceled

                            UploadWithErrors _ ->
                                Decode.succeed CsvUploadCanceled

                            UploadReady _ ->
                                Decode.succeed CsvUploadAccepted

                            Idle ->
                                Decode.fail "upload not ready"

                            Fetching ->
                                Decode.fail "upload not ready"

                    _ ->
                        Decode.fail "wrong key"
            )
            (Decode.field "which" Decode.int)
        )



-- URL PARSING


parseQuery : String -> List ( String, String )
parseQuery queryString =
    String.split "&" queryString |> List.filterMap parseQueryHelp


parseQueryHelp : String -> Maybe ( String, String )
parseQueryHelp fragment =
    case String.split "=" fragment of
        [ key, val ] ->
            Url.percentDecode val
                |> Maybe.map2 Tuple.pair (Url.percentDecode key)

        _ ->
            Nothing


parseOrder : String -> SortOrder
parseOrder fragment =
    case String.split "." fragment of
        [ col, "asc" ] ->
            Asc col

        [ col, "desc" ] ->
            Desc col

        _ ->
            Unordered



-- UTILS


orderToQueryParams : SortOrder -> List QueryParameter
orderToQueryParams order =
    case order of
        Asc column ->
            [ Url.string "order" <| column ++ "." ++ "asc" ]

        Desc column ->
            [ Url.string "order" <| column ++ "." ++ "desc" ]

        Unordered ->
            []


parentReference :
    Table
    -> Maybe { tableName : String, id : String }
    -> Maybe ( String, String )
parentReference table parent =
    Dict.toList table.columns
        |> List.filterMap
            (\( colName, { constraint } ) ->
                case constraint of
                    ForeignKey foreignKey ->
                        case parent of
                            Just { tableName, id } ->
                                if foreignKey.tableName == tableName then
                                    Just ( colName, id )

                                else
                                    Nothing

                            Nothing ->
                                Nothing

                    _ ->
                        Nothing
            )
        |> List.head


isColumnVisible : ( String, Column ) -> Bool
isColumnVisible ( _, column ) =
    case column.columnType of
        TextCol ->
            False

        JsonCol ->
            False

        UuidCol ->
            False

        ObjectCol ->
            False

        ArrayCol _ ->
            False

        OtherCol _ _ ->
            False

        _ ->
            True



-- Dict.values table.columns
--     |> List.filterMap Client.associationJoin
--     |> (++)
--         (isColumnVisible table
--             |> Dict.keys
--             |> List.map PG.attribute
--         )


hasErrors _ =
    False



-- TEMPORARY HELPER FUNCTIONS
-- These replace functions from removed modules (Internal.Record, Internal.Field, Internal.Value)


recordId : Table -> Record -> Maybe String
recordId table record =
    Schema.tablePrimaryKeyName table
        |> Maybe.andThen (\pkName -> Dict.get pkName record)
        |> Maybe.andThen valueToString


recordTableName : Table -> String
recordTableName table =
    table.name


valueToString : Value -> Maybe String
valueToString val =
    case val of
        Schema.String s ->
            Just s

        Schema.Int i ->
            Just (String.fromInt i)

        Schema.Float f ->
            Just (String.fromFloat f)

        Schema.Bool b ->
            Just
                (if b then
                    "true"

                 else
                    "false"
                )

        Schema.Blank ->
            Nothing

        _ ->
            Nothing


fieldValueToHtml : Value -> Html Msg
fieldValueToHtml value =
    -- Stub: just display the value as text
    Html.text (valueToString value |> Maybe.withDefault "")


updateWithString : String -> String -> Record -> Record
updateWithString fieldName stringValue record =
    -- Stub: just insert as string value
    Dict.insert fieldName (String stringValue) record


primaryKey : Table -> Record -> Maybe { value : Value }
primaryKey table record =
    Schema.tablePrimaryKeyName table
        |> Maybe.andThen (\pkName -> Dict.get pkName record)
        |> Maybe.map (\value -> { value = value })

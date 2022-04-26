module Listing exposing
    ( Listing
    , Msg
    , ascendingBy
    , descendingBy
    , fetch
    , hideSearch
    , init
    , isSearchVisible
    , mapMsg
    , showSearch
    , update
    , view
    )

import Basics.Extra exposing (flip)
import Browser.Dom as Dom exposing (Viewport)
import Browser.Navigation as Nav
import Dict
import Html
    exposing
        ( Html
        , a
        , aside
        , button
        , div
        , h1
        , header
        , i
        , section
        , span
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes
    exposing
        ( attribute
        , class
        , classList
        , disabled
        , href
        , id
        , target
        )
import Html.Events as Events
    exposing
        ( on
        , onClick
        , onMouseDown
        , onMouseUp
        )
import Inflect as String
import Json.Decode as Decode
import Postgrest.Client as PG
import Postgrest.Download as Download exposing (Download, Format(..))
import Postgrest.Field as Field exposing (Field)
import Postgrest.Resource as Resource exposing (Resource)
import Postgrest.Resource.Client as Client exposing (Client)
import Postgrest.Schema exposing (Column, Constraint(..), Table)
import Postgrest.Value as Value exposing (Value(..))
import PostgrestAdmin.AuthScheme as AuthScheme
import PostgrestAdmin.OuterMsg as OuterMsg exposing (OuterMsg)
import Search exposing (Search)
import String.Extra as String
import Task exposing (Task)
import Time
import Time.Extra as Time
import Url
import Url.Builder as Url exposing (QueryParameter)
import Utils.Task exposing (Error(..), attemptWithError, fail)


type Page
    = Page (List Resource)
    | Blank


type SortOrder
    = Asc String
    | Desc String
    | Unordered


type Msg
    = ResourceLinkClicked String String
    | Fetched (List Resource)
    | ApplyFilters
    | Sort SortOrder
    | Reload
    | Scrolled
    | ScrollInfo (Result Dom.Error Viewport)
    | SearchChanged Search.Msg
    | SelectEnter
    | SelectOn
    | SelectOff
    | DownloadRequested Format
    | Downloaded Download
    | ToggleSearchOpen
    | FetchFailed Error


type TextSelect
    = Enter
    | On
    | Off


type alias Listing =
    { resourcesName : String
    , scrollPosition : Float
    , table : Table
    , pages : List Page
    , page : Int
    , order : SortOrder
    , search : Search
    , searchOpen : Bool
    , textSelect : TextSelect
    }


type alias EventConfig =
    { stopPropagation : Bool
    , preventDefault : Bool
    , message : Msg
    }


init : String -> Maybe String -> Table -> Listing
init resourcesName rawQuery table =
    let
        query =
            Maybe.map parseQuery rawQuery |> Maybe.withDefault []

        order =
            query
                |> (List.filter (Tuple.first >> (==) "order") >> List.head)
                |> Maybe.map (Tuple.second >> parseOrder)
                |> Maybe.withDefault Unordered
    in
    { resourcesName = resourcesName
    , page = 0
    , scrollPosition = 0
    , table = table
    , pages = []
    , order = order
    , search = Search.init table (rawQuery |> Maybe.withDefault "")
    , searchOpen = False
    , textSelect = Off
    }


mapMsg : Msg -> OuterMsg
mapMsg msg =
    case msg of
        FetchFailed err ->
            OuterMsg.RequestFailed err

        _ ->
            OuterMsg.Pass


isSearchVisible : Listing -> Bool
isSearchVisible { searchOpen, search } =
    searchOpen || Search.isBlank search


showSearch : Listing -> Listing
showSearch listing =
    { listing | searchOpen = True }


hideSearch : Listing -> Listing
hideSearch listing =
    { listing | searchOpen = False }


ascendingBy : String -> Listing -> Listing
ascendingBy column listing =
    { listing | order = Asc column }


descendingBy : String -> Listing -> Listing
descendingBy column listing =
    { listing | order = Desc column }


fetch : Client a -> Listing -> Cmd Msg
fetch client listing =
    fetchTask client listing
        |> attemptWithError FetchFailed Fetched


fetchTask : Client a -> Listing -> Task Error (List Resource)
fetchTask client listing =
    let
        { search, resourcesName, page, table, order } =
            listing

        pgOrder =
            Dict.toList table
                |> List.filterMap (sortBy resourcesName order)

        params =
            [ PG.select <| Client.selects table
            , PG.limit perPage
            , PG.offset (perPage * page)
            ]
    in
    case AuthScheme.toJwt client.authScheme of
        Just token ->
            Client.fetchMany client table resourcesName
                |> PG.setParams (params ++ pgOrder ++ Search.toPGQuery search)
                |> PG.toTask token
                |> Task.mapError PGError

        Nothing ->
            Task.fail AuthError


update : Client { a | key : Nav.Key } -> Msg -> Listing -> ( Listing, Cmd Msg )
update client msg listing =
    case msg of
        ResourceLinkClicked resourcesName id ->
            ( listing
            , Nav.pushUrl client.key <| Url.absolute [ resourcesName, id ] []
            )

        Fetched records ->
            let
                pages =
                    case listing.pages of
                        Blank :: ps ->
                            if List.length records < perPage then
                                Blank :: Page records :: ps

                            else
                                Page records :: ps

                        _ ->
                            Page records :: listing.pages
            in
            ( { listing | page = listing.page + 1, pages = pages }
            , Cmd.none
            )

        ApplyFilters ->
            ( listing
            , Dom.setViewportOf listing.resourcesName 0 0
                |> Task.attempt (always Reload)
            )

        Sort order ->
            ( { listing | order = order }
            , Dom.setViewportOf listing.resourcesName 0 0
                |> Task.attempt (always Reload)
            )

        Reload ->
            ( listing, listingPath listing |> Nav.pushUrl client.key )

        Scrolled ->
            ( listing
            , Dom.getViewportOf listing.resourcesName
                |> Task.attempt ScrollInfo
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
                                    | scrollPosition =
                                        viewport.viewport.y
                                  }
                                , Cmd.none
                                )

                            _ ->
                                fetchListing client
                                    { listing
                                        | scrollPosition = viewport.viewport.y
                                        , pages = Blank :: listing.pages
                                    }

                    else
                        ( { listing | scrollPosition = viewport.viewport.y }
                        , Cmd.none
                        )

                Err _ ->
                    ( listing, Cmd.none )

        SearchChanged searchMsg ->
            Search.update searchMsg listing.search
                |> Tuple.mapFirst (\search -> { listing | search = search })
                |> Tuple.mapSecond (searchChanged searchMsg)

        ToggleSearchOpen ->
            ( { listing | searchOpen = not listing.searchOpen }, Cmd.none )

        SelectEnter ->
            ( { listing | textSelect = Enter }, Cmd.none )

        SelectOff ->
            ( { listing | textSelect = Off }, Cmd.none )

        SelectOn ->
            ( { listing | textSelect = On }, Cmd.none )

        DownloadRequested format ->
            ( listing
            , Download.init format (listingPath listing)
                |> Download.fetch client
                |> attemptWithError FetchFailed Downloaded
            )

        Downloaded download ->
            ( listing, Download.save listing.resourcesName download )

        FetchFailed _ ->
            ( listing, Cmd.none )


listingPath : Listing -> String
listingPath { order, resourcesName, search } =
    let
        queryParams =
            orderToQueryParams order

        baseUrl =
            Url.absolute [ resourcesName ]
                (orderToQueryParams order)

        filterQuery =
            Search.toPGQuery search |> PG.toQueryString

        joinChar =
            if List.isEmpty queryParams then
                "?"

            else
                "&"
    in
    [ baseUrl, filterQuery ]
        |> List.filterMap String.nonBlank
        |> String.join joinChar


fetchListing : Client a -> Listing -> ( Listing, Cmd Msg )
fetchListing client listing =
    ( listing, fetch client listing )


scrollingDown : Viewport -> Listing -> Bool
scrollingDown { viewport } { scrollPosition } =
    scrollPosition < viewport.y


closeToBottom : Viewport -> Bool
closeToBottom { scene, viewport } =
    scene.height - viewport.y < (viewport.height * 2)


searchChanged : Search.Msg -> Cmd Search.Msg -> Cmd Msg
searchChanged msg cmd =
    if Search.isApplyMsg msg then
        Time.now |> Task.perform (always ApplyFilters)

    else
        Cmd.map SearchChanged cmd



-- View


view : Listing -> Html Msg
view listing =
    let
        fields =
            Dict.toList listing.table
                |> List.sortWith Field.compareTuple
                |> List.map Tuple.first

        body =
            tableHeading listing fields
                :: pagesFold listing fields [] 0 listing.pages
    in
    section
        [ class "resources-listing" ]
        [ listHeader listing.resourcesName
        , div
            [ id listing.resourcesName
            , class "resources-listing-results"
            , case listing.pages of
                Blank :: _ ->
                    class ""

                _ ->
                    Events.on "scroll" (Decode.succeed Scrolled)
            ]
            [ Html.table [] body
            ]
        , aside
            [ class "listing-controls" ]
            [ div
                [ class "controls" ]
                [ div
                    [ class "downloads" ]
                    [ button
                        [ class "button-clear"
                        , onClick (DownloadRequested CSV)
                        ]
                        [ text "Download CSV" ]
                    , button
                        [ class "button-clear"
                        , onClick (DownloadRequested JSON)
                        ]
                        [ text "Download JSON" ]
                    ]
                , div
                    []
                    [ if Search.isBlank listing.search then
                        text ""

                      else
                        toggleSearchButton listing
                    , button
                        [ onClick ApplyFilters
                        , disabled (Search.isBlank listing.search)
                        ]
                        [ text "Apply Filters" ]
                    ]
                ]
            , Html.map SearchChanged <|
                Search.view (isSearchVisible listing) listing.search
            ]
        ]


toggleSearchButton : Listing -> Html Msg
toggleSearchButton listing =
    button
        [ class "toggle-button"
        , class "button-clear"
        , classList [ ( "open", isSearchVisible listing ) ]
        , onClick ToggleSearchOpen
        ]
        [ i [ class "icono-play" ] []
        , if isSearchVisible listing then
            text "Hide"

          else
            text "Show"
        , text " Filters"
        ]


listHeader : String -> Html Msg
listHeader resourcesName =
    header []
        [ h1 [] [ text <| String.humanize resourcesName ]
        , div []
            [ a
                [ class "button"
                , href <| Url.absolute [ resourcesName, "new" ] []
                ]
                [ text "New Record" ]
            ]
        ]


tableHeading : Listing -> List String -> Html Msg
tableHeading listing fields =
    thead []
        [ tr [] <| List.map (tableHeader listing) fields ]


tableHeader : Listing -> String -> Html Msg
tableHeader { order } name =
    let
        defaultHeader =
            span
                [ class "sort"
                , attribute "aria-sort" "other"
                , onClick <| Sort <| Asc name
                ]
                [ text <| String.humanize name
                , i [ class "icono-play" ] []
                ]
    in
    th []
        [ case order of
            Asc col ->
                if col == name then
                    span
                        [ class "sort"
                        , attribute "aria-sort" "ascending"
                        , onClick <| Sort <| Desc name
                        ]
                        [ text <| String.humanize name
                        , i [ class "asc icono-play" ] []
                        ]

                else
                    defaultHeader

            Desc col ->
                if col == name then
                    span
                        [ class "sort"
                        , attribute "aria-sort" "descending"
                        , onClick <| Sort <| Asc name
                        ]
                        [ text <| String.humanize name
                        , i [ class "desc icono-play" ] []
                        ]

                else
                    defaultHeader

            Unordered ->
                defaultHeader
        ]


pagesFold :
    Listing
    -> List String
    -> List (Html Msg)
    -> Int
    -> List Page
    -> List (Html Msg)
pagesFold listing fields acc pageNum pages =
    case pages of
        [] ->
            acc

        page :: rest ->
            let
                elem =
                    case page of
                        Page resources ->
                            viewPage listing fields pageNum resources

                        Blank ->
                            text ""
            in
            pagesFold listing fields (elem :: acc) (pageNum + 1) rest


viewPage : Listing -> List String -> Int -> List Resource -> Html Msg
viewPage listing fields pageNum records =
    tbody [ id <| pageId pageNum ] <|
        List.map (row listing fields) records


row : Listing -> List String -> Resource -> Html Msg
row ({ resourcesName, textSelect } as listing) names record =
    let
        toTd =
            fieldToHtml listing >> List.singleton >> td []

        id =
            Resource.id record |> Maybe.withDefault ""
    in
    List.filterMap (flip Dict.get record >> Maybe.map toTd) names
        |> tr
            [ class "listing-row"
            , onMouseDown SelectEnter
            , if textSelect == Enter then
                on "mousemove" (Decode.succeed SelectOn)

              else
                class ""
            , onMouseUp SelectOff
            , clickResource resourcesName textSelect id
            ]


fieldToHtml : Listing -> Field -> Html Msg
fieldToHtml { resourcesName, textSelect } { constraint, value } =
    case constraint of
        PrimaryKey ->
            recordLink resourcesName textSelect value Nothing

        ForeignKey { table, label } ->
            recordLink table textSelect value label

        NoConstraint ->
            valueToHtml value


valueToHtml : Value -> Html Msg
valueToHtml value =
    case value of
        PFloat (Just float) ->
            text (String.fromFloat float)

        PInt (Just int) ->
            text (String.fromInt int)

        PString (Just string) ->
            text string

        PEnum (Just string) _ ->
            text (String.humanize string)

        PBool (Just True) ->
            text "true"

        PBool (Just False) ->
            text "false"

        PTime (Just time) ->
            text (Time.format time)

        PDate (Just time) ->
            text (Time.toDateString time)

        PText _ ->
            text "..."

        Unknown _ ->
            text "?"

        _ ->
            text ""


recordLink : String -> TextSelect -> Value -> Maybe String -> Html Msg
recordLink resourcesName textSelect value mtext =
    let
        id =
            Value.toString value |> Maybe.withDefault ""
    in
    a
        [ href <| Url.absolute [ resourcesName, id ] []
        , target "_self"
        , clickResource resourcesName textSelect id
        ]
        [ Maybe.map text mtext |> Maybe.withDefault (valueToHtml value) ]


clickResource : String -> TextSelect -> String -> Html.Attribute Msg
clickResource resourcesName textSelect id =
    let
        msg =
            if textSelect == On then
                SelectOff

            else
                ResourceLinkClicked resourcesName id
    in
    Events.custom "click" <|
        Decode.map (EventConfig True True)
            (Decode.succeed msg)



-- Http interactions


pageId : Int -> String
pageId pageNum =
    "page-" ++ String.fromInt pageNum


perPage : Int
perPage =
    50


sortBy : String -> SortOrder -> ( String, Column ) -> Maybe PG.Param
sortBy resourcesName sort ( name, { constraint } ) =
    let
        colName =
            case constraint of
                ForeignKey params ->
                    params.labelColumnName
                        |> Maybe.map
                            (\columnName ->
                                String.join "_"
                                    [ resourcesName, params.table, columnName ]
                            )
                        |> Maybe.withDefault name

                _ ->
                    name
    in
    case sort of
        Asc f ->
            if f == name then
                Just <| PG.order [ PG.asc colName |> PG.nullslast ]

            else
                Nothing

        Desc f ->
            if f == name then
                Just <| PG.order [ PG.desc colName |> PG.nullslast ]

            else
                Nothing

        Unordered ->
            Nothing



-- Url parsing


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
        [ table, "asc" ] ->
            Asc table

        [ table, "desc" ] ->
            Desc table

        _ ->
            Unordered


orderToQueryParams : SortOrder -> List QueryParameter
orderToQueryParams order =
    case order of
        Asc column ->
            [ Url.string "order" <| column ++ "." ++ "asc" ]

        Desc column ->
            [ Url.string "order" <| column ++ "." ++ "desc" ]

        Unordered ->
            []

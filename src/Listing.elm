module Listing exposing
    ( Listing
    , Msg
    , ascendingBy
    , descendingBy
    , fetch
    , hideSearch
    , init
    , isSearchVisible
    , outerMsg
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
        , href
        , id
        , target
        )
import Html.Events as Events exposing (on, onClick, onMouseDown, onMouseUp)
import Inflect as String
import Json.Decode as Decode
import Postgrest.Client as PG
import Postgrest.Field exposing (Field)
import Postgrest.PrimaryKey as PrimaryKey exposing (PrimaryKey)
import Postgrest.Resource as Resource exposing (Resource)
import Postgrest.Resource.Client as Client exposing (Client)
import Postgrest.Schema.Definition exposing (Column(..), Definition)
import Postgrest.Value exposing (Value(..))
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
    | ToggleSearchOpen
    | FetchFailed Error


type TextSelect
    = Enter
    | On
    | Off


type alias Listing =
    { resourcesName : String
    , scrollPosition : Float
    , definition : Definition
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


init : String -> Maybe String -> Definition -> Listing
init resourcesName rawQuery definition =
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
    , definition = definition
    , pages = []
    , order = order
    , search = Search.init definition (rawQuery |> Maybe.withDefault "")
    , searchOpen = False
    , textSelect = Off
    }


outerMsg : Msg -> OuterMsg
outerMsg msg =
    case msg of
        FetchFailed err ->
            OuterMsg.RequestFailed err

        _ ->
            OuterMsg.Pass


isSearchVisible : Listing -> Bool
isSearchVisible { searchOpen } =
    searchOpen


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


fetch : Client a -> Listing -> ( Listing, Cmd Msg )
fetch client listing =
    ( listing
    , fetchResources client listing
        |> attemptWithError FetchFailed Fetched
    )


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
            let
                queryParams =
                    orderToQueryParams listing.order

                baseUrl =
                    Url.absolute [ listing.resourcesName ]
                        (orderToQueryParams listing.order)

                filterQuery =
                    Search.toPGQuery listing.search |> PG.toQueryString

                joinChar =
                    if List.isEmpty queryParams then
                        "?"

                    else
                        "&"

                url =
                    [ baseUrl, filterQuery ]
                        |> List.filterMap String.nonBlank
                        |> String.join joinChar
            in
            ( listing, Nav.pushUrl client.key url )

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
                                fetch client
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

        FetchFailed _ ->
            ( listing, Cmd.none )


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



-- Cmd.map SearchChanged cmd
-- View


view : Listing -> Html Msg
view listing =
    let
        fields =
            Dict.toList listing.definition
                |> List.sortWith sortColumns
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
        , aside [ class "listing-controls" ]
            [ div [ class "controls" ]
                [ button
                    [ class "toggle-button"
                    , class "button-clear"
                    , classList [ ( "open", listing.searchOpen ) ]
                    , onClick ToggleSearchOpen
                    ]
                    [ i [ class "icono-play" ] []
                    , if listing.searchOpen then
                        text "Hide"

                      else
                        text "Show"
                    , text " Filters"
                    ]
                , button [ onClick ApplyFilters ] [ text "Apply Filters" ]
                ]
            , Html.map SearchChanged <|
                Search.view listing.searchOpen listing.search
            ]
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
                [ text <| "New " ++ String.singularize resourcesName ]
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
            viewValue listing >> List.singleton >> td []

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


viewValue : Listing -> Field -> Html Msg
viewValue { resourcesName, textSelect } { value } =
    case value of
        PFloat (Just float) ->
            text <| String.fromFloat float

        PInt (Just int) ->
            text <| String.fromInt int

        PString (Just string) ->
            text string

        PEnum (Just string) _ ->
            text <| String.humanize string

        PBool (Just True) ->
            text "true"

        PBool (Just False) ->
            text "false"

        PTime (Just time) ->
            text <| Time.format time

        PDate (Just time) ->
            text <| Time.toDateString time

        PForeignKey (Just primaryKey) { table, label } ->
            recordLink table textSelect primaryKey label

        PPrimaryKey (Just primaryKey) ->
            recordLink resourcesName textSelect primaryKey Nothing

        BadValue _ ->
            text "?"

        _ ->
            text ""


recordLink : String -> TextSelect -> PrimaryKey -> Maybe String -> Html Msg
recordLink resourcesName textSelect primaryKey mtext =
    let
        id =
            PrimaryKey.toString primaryKey
    in
    a
        [ href <| Url.absolute [ resourcesName, id ] []
        , target "_self"
        , clickResource resourcesName textSelect id
        ]
        [ text <| Maybe.withDefault id mtext ]


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


fetchResources : Client a -> Listing -> Task Error (List Resource)
fetchResources client listing =
    let
        { search, resourcesName, page, definition, order } =
            listing

        pgOrder =
            Dict.toList definition
                |> List.filterMap (sortBy resourcesName order)

        params =
            [ PG.select <| Client.selects definition
            , PG.limit perPage
            , PG.offset (perPage * page)
            ]
    in
    case AuthScheme.toJwt client.authScheme of
        Just token ->
            Client.fetchMany client definition resourcesName
                |> PG.setParams (params ++ pgOrder ++ Search.toPGQuery search)
                |> PG.toTask token
                |> Task.mapError PGError

        Nothing ->
            Task.fail AuthError


sortBy : String -> SortOrder -> ( String, Column ) -> Maybe PG.Param
sortBy resourcesName sort ( name, Column _ value ) =
    let
        colName =
            case value of
                PForeignKey _ params ->
                    params.labelColumnName
                        |> Maybe.map
                            (\cn ->
                                [ resourcesName, params.table, cn ]
                                    |> String.join "_"
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



-- Sort


sortColumns : ( String, Column ) -> ( String, Column ) -> Order
sortColumns ( name, Column _ val ) ( name_, Column _ val_ ) =
    sortValues ( name, val ) ( name_, val_ )


sortValues : ( String, Value ) -> ( String, Value ) -> Order
sortValues ( name, a ) ( _, b ) =
    case ( a, b ) of
        ( PPrimaryKey _, _ ) ->
            LT

        ( _, PPrimaryKey _ ) ->
            GT

        ( PForeignKey _ _, _ ) ->
            LT

        ( _, PForeignKey _ _ ) ->
            GT

        ( PString _, _ ) ->
            recordIdentifiers
                |> List.indexedMap (flip Tuple.pair)
                |> Dict.fromList
                |> Dict.get name
                |> Maybe.map (toFloat >> flip compare (1 / 0))
                |> Maybe.withDefault GT

        _ ->
            EQ



-- Utils


recordIdentifiers : List String
recordIdentifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]

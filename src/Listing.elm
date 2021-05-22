module Listing exposing
    ( Listing
    , Msg
    , fetch
    , init
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
import Html.Attributes exposing (class, classList, href, id, target)
import Html.Events as Events exposing (onClick)
import Html.Lazy exposing (lazy4)
import Inflect as String
import Json.Decode as Decode
import Postgrest.Client as PG
import Postgrest.Field exposing (Field)
import Postgrest.PrimaryKey as PrimaryKey exposing (PrimaryKey)
import Postgrest.Resource as Resource exposing (Resource)
import Postgrest.Resource.Client as Client exposing (Client)
import Postgrest.Schema.Definition exposing (Column(..), Definition)
import Postgrest.Value exposing (Value(..))
import Process
import String.Extra as String
import Task
import Time.Extra as Time
import Url.Builder as Url
import Utils.Task exposing (Error(..), attemptWithError)


type Page
    = Page (List Resource)
    | Blank


type Sort
    = Asc String
    | Desc String
    | Unsorted


type Msg
    = ResourceLinkClicked String String
    | Fetched (List Resource)
    | Sort Sort
    | Scrolled
    | Info Viewport
    | Failed Error


type alias Listing =
    { resourcesName : String
    , page : Int
    , scrollPosition : Float
    , definition : Definition
    , pages : List Page
    , sort : Sort
    }


type alias EventConfig =
    { stopPropagation : Bool
    , preventDefault : Bool
    , message : Msg
    }


init : String -> Definition -> Listing
init resourcesName definition =
    { resourcesName = resourcesName
    , page = 0
    , scrollPosition = 0
    , definition = definition
    , pages = []
    , sort = Unsorted
    }


fetch : Client a -> Listing -> ( Listing, Cmd Msg )
fetch client listing =
    ( listing, fetchResources client listing )


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
            ( { listing
                | page = listing.page + 1
                , pages = pages
              }
            , Cmd.none
            )

        Sort sort ->
            ( { listing | sort = sort }, Cmd.none )

        Scrolled ->
            ( listing
            , Dom.getViewportOf listing.resourcesName
                |> Task.mapError DomError
                |> attemptWithError Failed Info
            )

        Info viewport ->
            if scrollingDown viewport listing && closeToBottom viewport then
                case listing.pages of
                    Blank :: _ ->
                        ( listing, Cmd.none )

                    _ ->
                        fetch client
                            { listing
                                | scrollPosition = viewport.viewport.y
                                , pages = Blank :: listing.pages
                            }

            else
                ( listing, Cmd.none )

        Failed _ ->
            ( listing, Cmd.none )


scrollingDown : Viewport -> Listing -> Bool
scrollingDown { scene, viewport } { scrollPosition } =
    scrollPosition < viewport.y


closeToBottom : Viewport -> Bool
closeToBottom { scene, viewport } =
    scene.height - viewport.y < (viewport.height * 2)


view : Listing -> Html Msg
view listing =
    let
        fields =
            Dict.toList listing.definition
                |> List.sortWith sortColumns
                |> List.map Tuple.first

        body =
            tableHeading listing fields
                :: pagesFold listing.resourcesName fields [] 0 listing.pages
    in
    section
        [ class "resources-listing" ]
        [ listHeader listing.resourcesName
        , div
            [ id listing.resourcesName
            , case listing.pages of
                Blank :: _ ->
                    class ""

                _ ->
                    Events.on "scroll" (Decode.succeed Scrolled)
            ]
            [ Html.table [] body
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
tableHeader { sort } name =
    let
        defaultSort =
            i [ class "sort icono-play", onClick <| Sort <| Desc name ] []
    in
    th []
        [ case sort of
            Asc col ->
                if col == name then
                    i
                        [ class "asc sort icono-play"
                        , onClick <| Sort <| Desc name
                        ]
                        []

                else
                    defaultSort

            Desc col ->
                if col == name then
                    i
                        [ class "desc sort icono-play"
                        , onClick <| Sort <| Asc name
                        ]
                        []

                else
                    defaultSort

            Unsorted ->
                defaultSort
        , text <| String.humanize name
        ]


pagesFold :
    String
    -> List String
    -> List (Html Msg)
    -> Int
    -> List Page
    -> List (Html Msg)
pagesFold rname fields acc pageNum pages =
    case pages of
        [] ->
            acc

        page :: rest ->
            let
                elem =
                    case page of
                        Page resources ->
                            lazy4 viewPage rname fields pageNum resources

                        Blank ->
                            text ""
            in
            pagesFold rname fields (elem :: acc) (pageNum + 1) rest


viewPage : String -> List String -> Int -> List Resource -> Html Msg
viewPage rname fields pageNum records =
    tbody [ id <| pageId pageNum ] <|
        List.map (row rname fields) records


row : String -> List String -> Resource -> Html Msg
row resourcesName names record =
    let
        toTd =
            viewValue resourcesName >> List.singleton >> td []

        id =
            Resource.id record |> Maybe.withDefault ""
    in
    List.filterMap (flip Dict.get record >> Maybe.map toTd) names
        |> tr
            [ class "listing-row"
            , clickResource resourcesName id
            ]


viewValue : String -> Field -> Html Msg
viewValue resourcesName { value } =
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
            recordLink table primaryKey label

        PPrimaryKey (Just primaryKey) ->
            recordLink resourcesName primaryKey Nothing

        BadValue _ ->
            text "?"

        _ ->
            text ""


recordLink : String -> PrimaryKey -> Maybe String -> Html Msg
recordLink resourcesName primaryKey mtext =
    let
        id =
            PrimaryKey.toString primaryKey
    in
    a
        [ href <| Url.absolute [ resourcesName, id ] []
        , target "_self"
        , clickResource resourcesName id
        ]
        [ text <| Maybe.withDefault id mtext ]


clickResource : String -> String -> Html.Attribute Msg
clickResource resourcesName id =
    let
        msg =
            ResourceLinkClicked resourcesName id
    in
    Events.custom "click" <|
        Decode.map (EventConfig True True) (Decode.succeed msg)


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


pageId : Int -> String
pageId pageNum =
    "page-" ++ String.fromInt pageNum


perPage : Int
perPage =
    50



-- Http interactions


fetchResources : Client a -> Listing -> Cmd Msg
fetchResources ({ jwt } as client) { resourcesName, page, definition, sort } =
    let
        params =
            [ PG.select <| Client.selects definition
            , PG.limit perPage
            , PG.offset (perPage * page)
            ]
    in
    Client.fetchMany client definition resourcesName
        |> PG.setParams params
        |> PG.toTask jwt
        |> Task.mapError PGError
        |> attemptWithError Failed Fetched


recordIdentifiers : List String
recordIdentifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]

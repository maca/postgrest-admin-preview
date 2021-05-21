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
        , section
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes exposing (class, href, id, target)
import Html.Events as Events
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
import String.Extra as String
import Task
import Time.Extra as Time
import Url.Builder as Url
import Utils.Task exposing (Error(..), attemptWithError)


type Msg
    = ResourceLinkClicked String String
    | Fetched (List Resource)
    | Scrolled
    | Info Viewport
    | Failed Error


type alias Listing =
    { resourcesName : String
    , page : Int
    , scrollPosition : Float
    , definition : Definition
    , resources : List (List Resource)
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
    , resources = []
    }


fetch : Client a -> Listing -> ( Listing, Cmd Msg )
fetch client listing =
    ( listing, fetchResources client listing )


update : Client { a | key : Nav.Key } -> Msg -> Listing -> ( Listing, Cmd Msg )
update client msg listing =
    case msg of
        ResourceLinkClicked resources id ->
            ( listing
            , Nav.pushUrl client.key <| Url.absolute [ resources, id ] []
            )

        Fetched records ->
            ( { listing
                | page = listing.page + 1
                , resources = records :: listing.resources
              }
            , Cmd.none
            )

        Scrolled ->
            ( listing
            , Dom.getViewportOf listing.resourcesName
                |> Task.mapError DomError
                |> attemptWithError Failed Info
            )

        Info { scene, viewport } ->
            let
                scrollingDown =
                    listing.scrollPosition < viewport.y

                closeToBottom =
                    scene.height - viewport.y < (viewport.height * 2)
            in
            if scrollingDown && closeToBottom then
                { listing | scrollPosition = viewport.y }
                    |> fetch client

            else
                ( listing, Cmd.none )

        Failed _ ->
            ( listing, Cmd.none )


view : Listing -> Html Msg
view { resources, resourcesName, definition } =
    let
        fields =
            Dict.toList definition
                |> List.sortWith sortColumns
                |> List.map Tuple.first

        toHeader =
            String.humanize >> text >> List.singleton >> th []

        header =
            thead [] [ tr [] <| List.map toHeader fields ]
    in
    section
        [ class "resources-listing" ]
        [ viewListHeader resourcesName
        , div
            [ Events.on "scroll" (Decode.succeed Scrolled)
            , id resourcesName
            ]
            [ Html.table
                []
                (header :: viewPagesFold resourcesName fields [] 0 resources)
            ]
        ]


viewPagesFold :
    String
    -> List String
    -> List (Html Msg)
    -> Int
    -> List (List Resource)
    -> List (Html Msg)
viewPagesFold rname fields acc pageNum pages =
    case pages of
        [] ->
            acc

        page :: rest ->
            let
                elem =
                    lazy4 viewPage rname fields pageNum page
            in
            viewPagesFold rname fields (elem :: acc) (pageNum + 1) rest


viewPage : String -> List String -> Int -> List Resource -> Html Msg
viewPage rname fields pageNum records =
    tbody [ id <| pageId pageNum ] <|
        List.map (viewRow rname fields) records


viewListHeader : String -> Html Msg
viewListHeader resources =
    header []
        [ h1 [] [ text <| String.humanize resources ]
        , div []
            [ a
                [ class "button"
                , href <| Url.absolute [ resources, "new" ] []
                ]
                [ text <| "New " ++ String.singularize resources ]
            ]
        ]


viewRow : String -> List String -> Resource -> Html Msg
viewRow resources names record =
    let
        toTd =
            viewValue resources >> List.singleton >> td []

        id =
            Resource.id record |> Maybe.withDefault ""
    in
    List.filterMap (flip Dict.get record >> Maybe.map toTd) names
        |> tr
            [ class "listing-row"
            , clickResource resources id
            ]


viewValue : String -> Field -> Html Msg
viewValue resources { value } =
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
            recordLink resources primaryKey Nothing

        BadValue _ ->
            text "?"

        _ ->
            text ""


recordLink : String -> PrimaryKey -> Maybe String -> Html Msg
recordLink resources primaryKey mtext =
    let
        id =
            PrimaryKey.toString primaryKey
    in
    a
        [ href <| Url.absolute [ resources, id ] []
        , target "_self"
        , clickResource resources id
        ]
        [ text <| Maybe.withDefault id mtext ]


clickResource : String -> String -> Html.Attribute Msg
clickResource resources id =
    let
        msg =
            ResourceLinkClicked resources id
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
fetchResources ({ jwt } as client) { resourcesName, page, definition } =
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

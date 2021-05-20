module Listing exposing
    ( Listing
    , Msg
    , Params
    , init
    , load
    , toParams
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


type Listing
    = Loading Params Definition (List (List Resource))
    | Idle Params Definition (List (List Resource))


type Msg
    = ResourceLinkClicked String String
    | Fetched (List Resource)
    | Scrolled
    | Info Viewport
    | Failed Error


type alias Params =
    { resources : String
    , page : Int
    , scrollPosition : Float
    }


type alias EventConfig =
    { stopPropagation : Bool
    , preventDefault : Bool
    , message : Msg
    }


init : String -> Definition -> Listing
init resources definition =
    Loading { resources = resources, page = 0, scrollPosition = 0 }
        definition
        []


load : Client a -> Listing -> ( Listing, Cmd Msg )
load client listing =
    let
        params =
            toParams listing

        definition =
            toDefinition listing
    in
    ( Loading params definition (toPages listing)
    , fetchResources client definition params
    )


update : Client { a | key : Nav.Key } -> Msg -> Listing -> ( Listing, Cmd Msg )
update client msg listing =
    case msg of
        ResourceLinkClicked resources id ->
            ( listing
            , Nav.pushUrl client.key <| Url.absolute [ resources, id ] []
            )

        Fetched records ->
            case listing of
                Loading params definition pages ->
                    ( Idle { params | page = params.page + 1 }
                        definition
                        (records :: pages)
                    , Cmd.none
                    )

                Idle _ _ _ ->
                    ( listing, Cmd.none )

        Scrolled ->
            ( listing
            , Dom.getViewportOf (listingId listing)
                |> Task.mapError DomError
                |> attemptWithError Failed Info
            )

        Info { scene, viewport } ->
            case listing of
                Loading _ _ _ ->
                    ( listing, Cmd.none )

                Idle params definition pages ->
                    let
                        listing_ =
                            Idle { params | scrollPosition = viewport.y }
                                definition
                                pages

                        scrollingDown =
                            params.scrollPosition < viewport.y

                        closeToBottom =
                            scene.height - viewport.y < (viewport.height * 2)
                    in
                    if scrollingDown && closeToBottom then
                        load client listing_

                    else
                        ( listing_, Cmd.none )

        Failed _ ->
            ( listing, Cmd.none )


view : Listing -> Html Msg
view listing =
    let
        { resources } =
            toParams listing

        definition =
            toDefinition listing

        pages =
            toPages listing

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
        [ viewListHeader resources
        , div
            [ if isIdle listing then
                Events.on "scroll" (Decode.succeed Scrolled)

              else
                class ""
            , id <| listingId listing
            ]
            [ Html.table
                []
                (header :: viewPagesFold resources fields [] 0 pages)
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


isIdle : Listing -> Bool
isIdle listing =
    case listing of
        Loading _ _ _ ->
            False

        Idle _ _ _ ->
            True


toParams : Listing -> Params
toParams listing =
    case listing of
        Loading params _ _ ->
            params

        Idle params _ _ ->
            params


toDefinition : Listing -> Definition
toDefinition listing =
    case listing of
        Loading _ definition _ ->
            definition

        Idle _ definition _ ->
            definition


toPages : Listing -> List (List Resource)
toPages listing =
    case listing of
        Loading _ _ pages ->
            pages

        Idle _ _ pages ->
            pages


listingId : Listing -> String
listingId listing =
    toParams listing |> .resources


pageId : Int -> String
pageId pageNum =
    "page-" ++ String.fromInt pageNum


perPage : Int
perPage =
    50



-- Http interactions


fetchResources : Client a -> Definition -> Params -> Cmd Msg
fetchResources ({ jwt } as client) definition { resources, page } =
    let
        params =
            [ PG.select <| Client.selects definition
            , PG.limit perPage
            , PG.offset (perPage * page)
            ]
    in
    Client.fetchMany client definition resources
        |> PG.setParams params
        |> PG.toTask jwt
        |> Task.mapError PGError
        |> attemptWithError Failed Fetched


recordIdentifiers : List String
recordIdentifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]

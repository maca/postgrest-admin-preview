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

import Array
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
        , table
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes exposing (class, href, id, target)
import Html.Events as Events
import Html.Keyed as Keyed
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
import Utils.Task exposing (Error(..), attemptWithError, fail)


type Listing
    = Requested Params
    | Loading Params Definition
    | Ready Params Definition (List (List Resource))


type Msg
    = ResourceLinkClicked String String
    | Fetched (List Resource)
    | Scrolled
    | Info Viewport
    | Failed Error


type alias Params =
    { resources : String
    , page : Int
    }


type alias EventConfig =
    { stopPropagation : Bool
    , preventDefault : Bool
    , message : Msg
    }


init : String -> Listing
init resources =
    Requested { resources = resources, page = 0 }


load : Client a -> Params -> Definition -> ( Listing, Cmd Msg )
load client params definition =
    ( Loading params definition
    , fetchResources client params.resources
    )


update : { a | key : Nav.Key } -> Listing -> Msg -> ( Listing, Cmd Msg )
update { key } listing msg =
    case msg of
        ResourceLinkClicked resources id ->
            ( listing, Nav.pushUrl key <| Url.absolute [ resources, id ] [] )

        Fetched records ->
            case listing of
                Loading params definition ->
                    ( Ready params definition [ records ], Cmd.none )

                Ready params definition pages ->
                    ( Ready params definition (records :: pages), Cmd.none )

                Requested params ->
                    ( Requested params, Cmd.none )

        Scrolled ->
            ( listing
            , Dom.getViewportOf (listingId listing)
                |> Task.mapError DomError
                |> attemptWithError Failed Info
            )

        Info ({ scene, viewport } as info) ->
            if scene.height - viewport.y > 2000 then
                ( listing, Cmd.none )

            else
                ( listing, Cmd.none )

        Failed _ ->
            ( listing, Cmd.none )


view : Listing -> Html Msg
view listing =
    case listing of
        Ready { resources } definition pages ->
            let
                fields =
                    Dict.toList definition
                        |> List.sortWith sortColumns
                        |> List.map Tuple.first

                toHeader =
                    String.humanize >> text >> List.singleton >> th []

                header =
                    ( "header", thead [] [ tr [] <| List.map toHeader fields ] )
            in
            section
                [ class "resources-listing" ]
                [ displayListHeader resources
                , div
                    [ Events.on "scroll" (Decode.succeed Scrolled)
                    , id <| listingId listing
                    ]
                    [ Keyed.node "table"
                        []
                        (header :: pagesFold resources fields [] 0 pages)
                    ]
                ]

        Requested _ ->
            text ""

        Loading _ _ ->
            text ""


pagesFold :
    String
    -> List String
    -> List ( String, Html Msg )
    -> Int
    -> List (List Resource)
    -> List ( String, Html Msg )
pagesFold rname fields acc pageNum pages =
    case pages of
        [] ->
            acc

        page :: rest ->
            let
                elem =
                    ( pageId pageNum, lazy4 viewPage rname fields pageNum page )
            in
            pagesFold rname fields (elem :: acc) (pageNum + 1) rest


viewPage : String -> List String -> Int -> List Resource -> Html Msg
viewPage rname fields pageNum records =
    tbody [ id <| pageId pageNum ] <|
        List.map (displayRow rname fields) records


displayListHeader : String -> Html Msg
displayListHeader resources =
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


displayRow : String -> List String -> Resource -> Html Msg
displayRow resources names record =
    let
        toTd =
            displayValue resources >> List.singleton >> td []

        id =
            Resource.id record |> Maybe.withDefault ""
    in
    List.filterMap (flip Dict.get record >> Maybe.map toTd) names
        |> tr
            [ class "listing-row"
            , clickResource resources id
            ]


displayValue : String -> Field -> Html Msg
displayValue resources { value } =
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


toParams : Listing -> Params
toParams listing =
    case listing of
        Requested params ->
            params

        Loading params _ ->
            params

        Ready params _ _ ->
            params


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


fetchResources : Client a -> String -> Cmd Msg
fetchResources ({ schema, jwt } as model) resources =
    case Dict.get resources schema of
        Just definition ->
            let
                params =
                    [ PG.select <| Client.selects definition
                    , PG.limit perPage
                    ]
            in
            Client.fetchMany model definition resources
                |> PG.setParams params
                |> PG.toTask jwt
                |> Task.mapError PGError
                |> attemptWithError Failed Fetched

        Nothing ->
            fail Failed <| BadSchema resources


recordIdentifiers : List String
recordIdentifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]

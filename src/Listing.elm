module Listing exposing (Listing(..), Msg, load, resourcesName, update, view)

import Basics.Extra exposing (flip)
import Browser.Navigation as Nav
import Dict
import Error exposing (Error(..))
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
import Html.Attributes exposing (class, href, target)
import Html.Events as Events
import Inflect as String
import Json.Decode as Decode
import Postgrest.Client as PG
import Postgrest.Field exposing (Field)
import Postgrest.PrimaryKey as PrimaryKey exposing (PrimaryKey)
import Postgrest.Resource as Resource exposing (Resource)
import Postgrest.Resource.Client as Client exposing (Client)
import Postgrest.Schema.Definition exposing (Column(..), Definition)
import Postgrest.Value exposing (Value(..))
import Result
import String.Extra as String
import Time.Extra as Time
import Url.Builder as Url


type Msg
    = ResourceLinkClicked String String
    | Fetched (Result Error (List Resource))
    | Failure (Result Error Never)


type Listing
    = ListingRequested String
    | ListingLoading String Definition
    | ListingReady String Definition (List Resource)


type alias EventConfig =
    { stopPropagation : Bool
    , preventDefault : Bool
    , message : Msg
    }


load : Client a -> String -> Definition -> ( Listing, Cmd Msg )
load client rname definition =
    ( ListingLoading rname definition
    , fetchResources rname client
    )


update : { a | key : Nav.Key } -> Listing -> Msg -> ( Listing, Cmd Msg )
update { key } listing msg =
    case msg of
        ResourceLinkClicked rname id ->
            ( listing
            , Nav.pushUrl key <| Url.absolute [ rname, id ] []
            )

        Fetched result ->
            case result of
                Ok records ->
                    case listing of
                        ListingLoading name definition ->
                            ( ListingReady name definition records
                            , Cmd.none
                            )

                        _ ->
                            ( listing, Cmd.none )

                Err _ ->
                    ( listing, Cmd.none )

        Failure _ ->
            ( listing, Cmd.none )


view : Listing -> Html Msg
view listing =
    case listing of
        ListingReady rname definition records ->
            displayListing definition rname records

        ListingRequested _ ->
            text ""

        ListingLoading _ _ ->
            text ""


displayListing : Definition -> String -> List Resource -> Html Msg
displayListing definition rname records =
    let
        fieldNames =
            Dict.toList definition
                |> List.sortWith sortColumns
                |> List.map Tuple.first

        toHeader =
            String.humanize >> text >> List.singleton >> th []
    in
    section
        []
        [ displayListHeader rname
        , table []
            [ thead [] [ tr [] <| List.map toHeader fieldNames ]
            , tbody [] <|
                List.map (displayRow rname fieldNames) records
            ]
        ]


displayListHeader : String -> Html Msg
displayListHeader rname =
    header []
        [ h1 [] [ text <| String.humanize rname ]
        , div []
            [ a
                [ class "button"
                , href <| Url.absolute [ rname, "new" ] []
                ]
                [ text <| "New " ++ String.singularize rname ]
            ]
        ]


displayRow : String -> List String -> Resource -> Html Msg
displayRow rname names record =
    let
        toTd =
            displayValue rname >> List.singleton >> td []

        id =
            Resource.id record |> Maybe.withDefault ""
    in
    List.filterMap (flip Dict.get record >> Maybe.map toTd) names
        |> tr
            [ class "listing-row"
            , clickResource rname id
            ]


displayValue : String -> Field -> Html Msg
displayValue rname { value } =
    case value of
        PFloat (Just float) ->
            text <| String.fromFloat float

        PInt (Just int) ->
            text <| String.fromInt int

        PString (Just string) ->
            text string

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
            recordLink rname primaryKey Nothing

        BadValue _ ->
            text "?"

        _ ->
            text ""


recordLink : String -> PrimaryKey -> Maybe String -> Html Msg
recordLink rname primaryKey mtext =
    let
        id =
            PrimaryKey.toString primaryKey
    in
    a
        [ href <| Url.absolute [ rname, id ] []
        , target "_self"
        , clickResource rname id
        ]
        [ text <| Maybe.withDefault id mtext ]


clickResource : String -> String -> Html.Attribute Msg
clickResource rname id =
    let
        msg =
            ResourceLinkClicked rname id
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


resourcesName : Listing -> String
resourcesName listing =
    case listing of
        ListingRequested name ->
            name

        ListingLoading name _ ->
            name

        ListingReady name _ _ ->
            name



-- Http interactions


fetchResources : String -> Client a -> Cmd Msg
fetchResources rname ({ schema, jwt } as model) =
    case Dict.get rname schema of
        Just definition ->
            Client.fetchMany model definition rname
                |> PG.toCmd jwt (Fetched << Result.mapError PGError)

        Nothing ->
            Error.fail Failure <| BadSchema rname



-- To refactor


recordIdentifiers : List String
recordIdentifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]

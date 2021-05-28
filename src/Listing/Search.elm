module Listing.Search exposing (Msg, Search, init, update, view)

import Array exposing (Array)
import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Html
    exposing
        ( Html
        , button
        , div
        , form
        , h3
        , h4
        , i
        , input
        , option
        , select
        , text
        )
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onClick, onInput)
import Listing.Search.Bool as Bool exposing (BoolOp)
import Listing.Search.Enum as Enum exposing (EnumOp(..))
import Listing.Search.Filter as Filter exposing (Filter(..))
import Listing.Search.Num as Num exposing (NumOp(..))
import Listing.Search.Text as Text exposing (TextOp(..))
import Listing.Search.Time as Time exposing (TimeOp(..))
import Postgrest.Schema.Definition exposing (Column(..), Definition)
import Postgrest.Value as Value exposing (Value(..))
import String.Extra as String


type Msg
    = UpdateFilter Int Filter
    | AddFilter


type alias Search =
    { definition : Definition
    , filters : Array Filter
    }


init : Definition -> Search
init definition =
    { definition = definition
    , filters = Array.empty
    }


update : Msg -> Search -> ( Search, Cmd Msg )
update msg search =
    case msg of
        UpdateFilter idx filter ->
            ( { search | filters = Array.set idx filter search.filters }
            , Cmd.none
            )

        AddFilter ->
            let
                filter =
                    search.definition
                        |> Dict.toList
                        |> List.head
                        |> Maybe.map (\( n, c ) -> Filter.fromColumn n c)
                        |> Maybe.withDefault Blank
            in
            ( { search | filters = Array.push filter search.filters }
            , Cmd.none
            )


view : Search -> Html Msg
view { definition, filters } =
    let
        _ =
            Debug.log "filters" filters
    in
    div
        []
        ([ h3 [] [ text "filter" ] ]
            ++ (Array.indexedMap (viewFilter definition) filters |> Array.toList)
            ++ [ button [ onClick AddFilter ] [ i [ class "icono-plus" ] [] ] ]
        )


viewFilter definition idx filter =
    let
        inputs name content =
            div
                [ class "filter"
                , class <| Filter.toString filter
                ]
                ([ fieldSelect definition idx name filter ] ++ content)
    in
    case filter of
        TextFilter name op ->
            inputs name <|
                List.map (Html.map (TextFilter name >> UpdateFilter idx)) <|
                    [ Text.select op, Text.input op ]

        NumFilter name op ->
            Debug.todo "crash"

        Blank ->
            text ""

        _ ->
            text ""


fieldSelect : Definition -> Int -> String -> Filter -> Html Msg
fieldSelect definition idx name filter =
    let
        makeFilter selection =
            let
                filter_ =
                    defaultFilter selection definition
            in
            if Filter.toString filter_ == Filter.toString filter then
                Filter.reassign selection filter

            else
                filter_
    in
    select
        [ onInput (makeFilter >> UpdateFilter idx) ]
        (Dict.keys definition
            |> List.map
                (\s ->
                    option
                        [ selected (s == name), value s ]
                        [ text <| String.humanize s ]
                )
        )


defaultFilter : String -> Definition -> Filter
defaultFilter colName definition =
    Dict.get colName definition
        |> Maybe.map (Filter.fromColumn colName)
        |> Maybe.withDefault Blank

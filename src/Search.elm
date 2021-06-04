module Search exposing (Msg, Search, init, toPGQuery, update, view)

import Array exposing (Array)
import Array.Extra as Array
import Dict
import Filter as Filter exposing (Filter(..), Kind(..))
import Filter.Operation
    exposing
        ( boolFilterInputs
        , dateFilterInputs
        , enumInputs
        , floatFilterInputs
        , intFilterInputs
        , textFilterInputs
        , timeFilterInputs
        )
import Html exposing (Html, button, div, i, option, select, text)
import Html.Attributes exposing (class, hidden, selected, title, value)
import Html.Events exposing (onClick, onInput)
import Postgrest.Client as PG
import Postgrest.Schema.Definition exposing (Column(..), Definition)
import Postgrest.Value exposing (Value(..))
import String.Extra as String


type Msg
    = UpdateFilter Int Filter
    | AddFilter
    | RemoveFilter Int


type alias Search =
    { definition : Definition
    , filters : Array Filter
    }


init : Definition -> String -> Search
init definition query =
    { definition = definition
    , filters =
        String.split "&" query
            |> List.filterMap (Filter.parse definition)
            |> Array.fromList
    }


toPGQuery : Search -> List PG.Param
toPGQuery { filters } =
    Array.toList filters |> List.filterMap Filter.toPGQuery


update : Msg -> Search -> ( Search, Cmd Msg )
update msg search =
    case msg of
        UpdateFilter idx filter ->
            ( { search | filters = Array.set idx filter search.filters }
            , Cmd.none
            )

        AddFilter ->
            let
                mfilter =
                    search.definition
                        |> Dict.toList
                        |> List.head
                        |> Maybe.andThen (\( n, c ) -> Filter.fromColumn n c)
            in
            ( { search
                | filters =
                    case mfilter of
                        Just filter ->
                            Array.push filter search.filters

                        Nothing ->
                            search.filters
              }
            , Cmd.none
            )

        RemoveFilter idx ->
            ( { search | filters = Array.removeAt idx search.filters }
            , Cmd.none
            )


view : Bool -> Search -> Html Msg
view open { definition, filters } =
    div [ class "search", hidden <| not open ]
        [ div [ class "actions" ]
            [ button
                [ onClick AddFilter
                , class "button-clear"
                , class "add-filter"
                ]
                [ text "Add filter", i [ class "icono-plus" ] [] ]
            ]
        , div
            [ class "filters" ]
            (Array.indexedMap (viewFilter definition) filters
                |> Array.toList
                |> List.reverse
            )
        ]


viewFilter : Definition -> Int -> Filter -> Html Msg
viewFilter definition idx ((Filter kind name op) as filter) =
    let
        isRequired =
            Dict.get name definition
                |> Maybe.map (\(Column req _) -> req)
                |> Maybe.withDefault False

        inputs sKind content =
            div
                [ class "filter"
                , class sKind
                ]
                [ div [ class "filter-inputs" ]
                    (fieldSelect definition idx filter :: content)
                , button
                    [ class "button-clear"
                    , onClick <| RemoveFilter idx
                    , title "Remove filter"
                    , class "filter-remove"
                    ]
                    [ i [ class "icono-cross" ] [] ]
                ]
    in
    case kind of
        IText ->
            textFilterInputs isRequired op
                |> List.map (Html.map (Filter IText name >> UpdateFilter idx))
                |> inputs "text"

        IInt ->
            intFilterInputs isRequired op
                |> List.map (Html.map (Filter IInt name >> UpdateFilter idx))
                |> inputs "number"

        IFloat ->
            floatFilterInputs isRequired op
                |> List.map (Html.map (Filter IFloat name >> UpdateFilter idx))
                |> inputs "number"

        IDate ->
            dateFilterInputs isRequired op
                |> List.map (Html.map (Filter IDate name >> UpdateFilter idx))
                |> inputs "date"

        ITime ->
            timeFilterInputs isRequired op
                |> List.map (Html.map (Filter ITime name >> UpdateFilter idx))
                |> inputs "time"

        IBool ->
            boolFilterInputs isRequired op
                |> List.map (Html.map (Filter IBool name >> UpdateFilter idx))
                |> inputs "bool"

        IEnum ->
            enumInputs isRequired idx op
                |> List.map (Html.map (Filter IEnum name >> UpdateFilter idx))
                |> inputs "enum"


fieldSelect : Definition -> Int -> Filter -> Html Msg
fieldSelect definition idx ((Filter kind name op) as filter) =
    let
        makeFilter selection =
            case defaultFilter selection definition of
                Just ((Filter kind_ name_ _) as filter_) ->
                    if kind_ == kind then
                        Filter kind name_ op

                    else
                        filter_

                Nothing ->
                    filter

        makeOption s =
            option
                [ value s, selected (s == name) ]
                [ text <| String.humanize s ]
    in
    select
        [ onInput (makeFilter >> UpdateFilter idx) ]
        (Dict.toList definition
            |> List.filterMap
                (\( s, column ) ->
                    Filter.fromColumn s column
                        |> Maybe.map (always <| makeOption s)
                )
        )


defaultFilter : String -> Definition -> Maybe Filter
defaultFilter colName definition =
    Dict.get colName definition
        |> Maybe.andThen (Filter.fromColumn colName)

module Search exposing (Msg, Search, init, update, view)

import Array exposing (Array)
import Array.Extra as Array
import Dict
import Filter as Filter exposing (Filter(..))
import Filter.Operator
    exposing
        ( boolFilterInputs
        , dateFilterInputs
        , enumInputs
        , floatFilterInputs
        , intFilterInputs
        , textFilterInputs
        , timeFilterInputs
        )
import Html
    exposing
        ( Html
        , aside
        , button
        , div
        , h3
        , i
        , input
        , option
        , select
        , text
        )
import Html.Attributes exposing (attribute, class, selected, title, value)
import Html.Events exposing (onClick, onInput)
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

        RemoveFilter idx ->
            ( { search | filters = Array.removeAt idx search.filters }
            , Cmd.none
            )


view : Search -> Html Msg
view { definition, filters } =
    let
        _ =
            Debug.log "filter" filters
    in
    aside
        [ class "filters" ]
        (h3 [] [ text "Filter" ]
            :: (Array.indexedMap (viewFilter definition) filters |> Array.toList)
            ++ [ button
                    [ onClick AddFilter
                    , class "button-clear"
                    , class "add-filter"
                    ]
                    [ text "Add filter", i [ class "icono-plus" ] [] ]
               ]
        )


viewFilter : Definition -> Int -> Filter -> Html Msg
viewFilter definition idx filter =
    let
        required name =
            Dict.get name definition
                |> Maybe.map (\(Column req _) -> req)
                |> Maybe.withDefault False

        inputs name content =
            div
                [ class "filter"
                , class <| Filter.toString filter
                ]
                [ div [ class "filter-inputs" ]
                    (fieldSelect definition idx name filter :: content)
                , button
                    [ class "button-clear"
                    , onClick <| RemoveFilter idx
                    , title "Remove filter"
                    , class "filter-remove"
                    ]
                    [ i [ class "icono-cross" ] [] ]
                ]
    in
    case filter of
        TextFilter name op ->
            textFilterInputs (required name) op
                |> List.map (Html.map (TextFilter name >> UpdateFilter idx))
                |> inputs name

        IntFilter name op ->
            intFilterInputs (required name) op
                |> List.map (Html.map (IntFilter name >> UpdateFilter idx))
                |> inputs name

        FloatFilter name op ->
            floatFilterInputs (required name) op
                |> List.map (Html.map (FloatFilter name >> UpdateFilter idx))
                |> inputs name

        DateFilter name op ->
            dateFilterInputs (required name) op
                |> List.map (Html.map (DateFilter name >> UpdateFilter idx))
                |> inputs name

        TimeFilter name op ->
            timeFilterInputs (required name) op
                |> List.map (Html.map (TimeFilter name >> UpdateFilter idx))
                |> inputs name

        BoolFilter name op ->
            boolFilterInputs (required name) op
                |> List.map (Html.map (BoolFilter name >> UpdateFilter idx))
                |> inputs name

        EnumFilter name choices op ->
            enumInputs (required name) choices idx op
                |> List.map
                    (Html.map (EnumFilter name choices >> UpdateFilter idx))
                |> inputs name

        Blank ->
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
                    if makeFilter s == Blank then
                        text ""

                    else
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

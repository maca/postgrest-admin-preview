module Search exposing (Msg, Search, init, toPGQuery, update, view)

import Array exposing (Array)
import Array.Extra as Array
import Basics.Extra exposing (flip)
import Dict
import Filter as Filter exposing (Filter(..), Kind(..))
import Filter.Operand as Operand
    exposing
        ( Enum(..)
        , Operand(..)
        , date
        , float
        , int
        , text
        , time
        )
import Filter.Operation as Operation exposing (Operation(..))
import Html exposing (Attribute, Html, a, button, div, i, label, option, span)
import Html.Attributes
    exposing
        ( checked
        , class
        , for
        , hidden
        , id
        , selected
        , step
        , title
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput)
import Postgrest.Client as PG
import Postgrest.Schema.Definition exposing (Column(..), Definition)
import Postgrest.Value exposing (Value(..))
import Set
import String.Extra as String
import Url exposing (percentDecode)


type Msg
    = UpdateFilter Int Filter
    | AddFilter
    | RemoveFilter Int


type alias OperationConst =
    String -> String -> Operation


type alias Options =
    List ( String, OperationConst )


type alias Search =
    { definition : Definition
    , filters : Array Filter
    }


init : Definition -> String -> Search
init definition query =
    { definition = definition
    , filters =
        String.split "&" query
            |> List.filterMap
                (percentDecode >> Maybe.andThen (Filter.parse definition))
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



-- View


view : Bool -> Search -> Html Msg
view open { definition, filters } =
    div [ class "search", hidden <| not open ]
        [ div [ class "actions" ]
            [ button
                [ onClick AddFilter
                , class "button-clear"
                , class "add-filter"
                ]
                [ Html.text "Add filter", i [ class "icono-plus" ] [] ]
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
                [ Html.text <| String.humanize s ]
    in
    Html.select
        [ onInput (makeFilter >> UpdateFilter idx) ]
        (Dict.toList definition
            |> List.filterMap
                (\( s, column ) ->
                    Filter.fromColumn s column
                        |> Maybe.map (always <| makeOption s)
                )
        )


textFilterInputs : Bool -> Operation -> List (Html Operation)
textFilterInputs required op =
    let
        options =
            List.map operationOption
                [ map Equals text
                , map Contains text
                , map StartsWith text
                , map EndsWith text
                ]
    in
    [ select (options ++ nullOption required op) op, input op ]


intFilterInputs : Bool -> Operation -> List (Html Operation)
intFilterInputs required op =
    let
        options =
            List.map operationOption
                [ map Equals int
                , map LesserThan int
                , map GreaterThan int
                , map2 Between int
                ]
    in
    [ select (options ++ nullOption required op) op, input op ]


floatFilterInputs : Bool -> Operation -> List (Html Operation)
floatFilterInputs required op =
    let
        options =
            List.map operationOption
                [ map Equals float
                , map LesserThan float
                , map GreaterThan float
                , map2 Between float
                ]
    in
    [ select (options ++ nullOption required op) op, input op ]


dateFilterInputs : Bool -> Operation -> List (Html Operation)
dateFilterInputs required op =
    let
        options =
            List.map operationOption
                [ map InDate date
                , map LesserThan date
                , map GreaterThan date
                , map2 Between date
                ]
    in
    [ select (options ++ nullOption required op) op, input op ]


timeFilterInputs : Bool -> Operation -> List (Html Operation)
timeFilterInputs required op =
    let
        options =
            List.map operationOption
                [ map InDate time
                , map LesserThan time
                , map GreaterThan time
                , map2 Between time
                ]
    in
    [ select (options ++ nullOption required op) op, input op ]


boolFilterInputs : Bool -> Operation -> List (Html Operation)
boolFilterInputs required op =
    let
        options =
            List.map (dropBoth >> operationOption) [ IsTrue, IsFalse ]
    in
    [ select (options ++ nullOption required op) op, input op ]


enumInputs : Bool -> Int -> Operation -> List (Html Operation)
enumInputs required idx op =
    let
        options enum =
            List.map operationOption
                [ map OneOf (flip choose enum)
                , map NoneOf (flip choose enum)
                ]

        inputs makeOp ((Enum choices _) as enum) =
            [ select (options enum ++ nullOption required op) op
            , choices
                |> List.map (checkbox makeOp idx enum)
                |> div [ class "checkboxes" ]
            ]
    in
    case op of
        OneOf enum ->
            inputs (map OneOf (flip choose enum)) enum

        NoneOf enum ->
            inputs (map NoneOf (flip choose enum)) enum

        IsNull (Just op_) ->
            case op_ of
                OneOf enum ->
                    [ select (options enum ++ nullOption required op_) op ]

                NoneOf enum ->
                    [ select (options enum ++ nullOption required op_) op ]

                _ ->
                    []

        _ ->
            []


choose : String -> Enum -> Enum
choose choice (Enum choices chosen) =
    if Set.member choice chosen then
        Enum choices (Set.remove choice chosen)

    else if String.isEmpty choice then
        Enum choices chosen

    else
        Enum choices (Set.insert choice chosen)


select : Options -> Operation -> Html Operation
select options op =
    let
        opSelect a b =
            let
                makeOption ( s, _ ) =
                    option [ selected (Operation.toString op == s) ] [ Html.text s ]

                optionSelected selection =
                    let
                        makeOperation =
                            Dict.fromList options
                                |> Dict.get selection
                                |> Maybe.withDefault (dropBoth <| IsNull <| Just op)
                    in
                    makeOperation a b
            in
            Html.select
                [ onInput optionSelected ]
                (List.map makeOption options)
    in
    case op of
        Equals a ->
            opSelect (Operand.value a) ""

        Contains a ->
            opSelect (Operand.value a) ""

        StartsWith a ->
            opSelect (Operand.value a) ""

        EndsWith a ->
            opSelect (Operand.value a) ""

        LesserThan a ->
            opSelect (Operand.value a) ""

        GreaterThan a ->
            opSelect (Operand.value a) ""

        Between a b ->
            opSelect (Operand.value a) (Operand.value b)

        InDate a ->
            opSelect (Operand.value a) ""

        OneOf _ ->
            opSelect "" ""

        NoneOf _ ->
            opSelect "" ""

        IsTrue ->
            opSelect "" ""

        IsFalse ->
            opSelect "" ""

        IsNull _ ->
            opSelect "" ""


input : Operation -> Html Operation
input op =
    let
        attributes operand =
            case operand of
                OText _ ->
                    [ type_ "text" ]

                OInt _ ->
                    [ type_ "number", step "1" ]

                OFloat _ ->
                    [ type_ "number", step "0.01" ]

                ODate _ ->
                    [ type_ "date" ]

                OTime _ ->
                    [ type_ "datetime-local" ]
    in
    case op of
        Equals a ->
            textInput (attributes a) Equals a

        Contains a ->
            textInput (attributes a) Contains a

        StartsWith a ->
            textInput (attributes a) StartsWith a

        EndsWith a ->
            textInput (attributes a) EndsWith a

        LesserThan a ->
            textInput (attributes a) LesserThan a

        GreaterThan a ->
            textInput (attributes a) GreaterThan a

        Between a b ->
            div []
                [ textInput (attributes a) (flip Between b) a
                , span [] [ Html.text "and" ]
                , textInput (attributes a) (Between a) b
                ]

        InDate a ->
            textInput [ type_ "date" ] InDate a

        OneOf _ ->
            Html.text ""

        NoneOf _ ->
            Html.text ""

        IsTrue ->
            Html.text ""

        IsFalse ->
            Html.text ""

        IsNull _ ->
            Html.text ""


textInput :
    List (Attribute Operation)
    -> (Operand -> Operation)
    -> Operand
    -> Html Operation
textInput attributes makeOperation operand =
    Html.input
        ([ onInput (makeOperation << Operand.constructor operand)
         , value <| Operand.rawValue operand
         ]
            ++ attributes
        )
        []


checkbox : OperationConst -> Int -> Enum -> String -> Html Operation
checkbox makeOp idx (Enum _ chosen) choice =
    let
        inputId =
            String.fromInt idx |> (++) choice
    in
    label
        [ for inputId ]
        [ Html.input
            [ id inputId
            , value choice
            , onInput (\s -> makeOp s "")
            , Html.Attributes.type_ "checkbox"
            , checked <| Set.member choice chosen
            ]
            []
        , Html.text <| String.humanize choice
        ]


nullOption : Bool -> Operation -> List ( String, OperationConst )
nullOption required op =
    if required then
        []

    else
        [ operationOption <| dropBoth (IsNull <| Just op) ]


operationOption : OperationConst -> ( String, OperationConst )
operationOption cons =
    ( Operation.toString (cons "" ""), cons )


defaultFilter : String -> Definition -> Maybe Filter
defaultFilter colName definition =
    Dict.get colName definition
        |> Maybe.andThen (Filter.fromColumn colName)



-- Utils


dropBoth : c -> a -> b -> c
dropBoth c _ _ =
    c


map : (op -> c) -> (s -> op) -> s -> s -> c
map cons mfun a _ =
    cons (mfun a)


map2 : (op -> op -> c) -> (s -> op) -> s -> s -> c
map2 cons mfun a b =
    cons (mfun a) (mfun b)

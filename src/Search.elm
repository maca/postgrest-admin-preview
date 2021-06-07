module Search exposing (Msg, Search, init, isApplyMsg, toPGQuery, update, view)

import Array exposing (Array)
import Array.Extra as Array
import Basics.Extra exposing (flip)
import Dict
import Filter as Filter exposing (Filter)
import Filter.Operand as Operand
    exposing
        ( Enum
        , Operand
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
import Html.Events exposing (keyCode, on, onClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy as Lazy
import Json.Decode as Decode
import Postgrest.Client as PG
import Postgrest.Schema.Definition exposing (Column(..), Definition)
import Postgrest.Value exposing (Value(..))
import Set
import String.Extra as String
import Url exposing (percentDecode)


type Msg
    = UpdateFilter Int Filter
    | AddFilter AddPosition
    | RemoveFilter Int
    | Apply


type AddPosition
    = Prepend
    | Append


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


isApplyMsg : Msg -> Bool
isApplyMsg msg =
    case msg of
        Apply ->
            True

        _ ->
            False


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

        AddFilter position ->
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
                            if position == Prepend then
                                search.filters
                                    |> Array.append (Array.fromList [ filter ])

                            else
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

        Apply ->
            ( search, Cmd.none )



-- View


view : Bool -> Search -> Html Msg
view open { definition, filters } =
    div [ class "search", hidden <| not open ]
        [ div [ class "actions" ] [ buttonAdd Prepend ]
        , Keyed.node "div"
            [ class "filters" ]
            (Array.indexedMap
                (\idx filter ->
                    ( String.fromInt idx
                    , Lazy.lazy3 viewFilter definition idx filter
                    )
                )
                filters
                |> Array.toList
            )
        , if Array.isEmpty filters then
            Html.text ""

          else
            div [ class "actions" ] [ buttonAdd Append ]
        ]


buttonAdd tagger =
    button
        [ onClick (AddFilter tagger)
        , class "button-clear"
        , class "add-filter"
        ]
        [ Html.text "Add filter", i [ class "icono-plus" ] [] ]


viewFilter : Definition -> Int -> Filter -> Html Msg
viewFilter definition idx filter =
    let
        name =
            Filter.columnName filter

        op =
            Filter.operation filter

        inputs kind content =
            div
                [ class "filter"
                , class kind
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
    case Dict.get name definition of
        Just (Column isRequired (PString _)) ->
            textFilterInputs isRequired name idx op
                |> inputs "text"

        Just (Column isRequired (PText _)) ->
            textFilterInputs isRequired name idx op
                |> inputs "text"

        Just (Column isRequired (PInt _)) ->
            intFilterInputs isRequired name idx op
                |> inputs "number"

        Just (Column isRequired (PFloat _)) ->
            floatFilterInputs isRequired name idx op
                |> inputs "date"

        Just (Column isRequired (PBool _)) ->
            boolFilterInputs isRequired name idx op
                |> inputs "bool"

        Just (Column isRequired (PTime _)) ->
            timeFilterInputs isRequired name idx op
                |> inputs "time"

        Just (Column isRequired (PDate _)) ->
            dateFilterInputs isRequired name idx op
                |> inputs "time"

        Just (Column isRequired (PEnum _ _)) ->
            enumInputs isRequired name idx op
                |> inputs "enum"

        _ ->
            Html.text ""


fieldSelect : Definition -> Int -> Filter -> Html Msg
fieldSelect definition idx filter =
    let
        op =
            Filter.operation filter

        makeFilter selection =
            case defaultFilter selection definition of
                Just filter_ ->
                    if
                        Operation.toString (Filter.operation filter_)
                            == Operation.toString op
                    then
                        Filter.init (Filter.columnName filter_) op

                    else
                        filter_

                Nothing ->
                    filter

        makeOption s =
            option
                [ value s, selected (s == Filter.columnName filter) ]
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


textFilterInputs : Bool -> String -> Int -> Operation -> List (Html Msg)
textFilterInputs required name idx op =
    let
        options =
            List.map operationOption
                [ map Filter.equals text
                , map Filter.contains text
                , map Filter.startsWith text
                , map Filter.endsWith text
                ]
    in
    [ select (options ++ nullOption required op) op
        |> Html.map (Filter.init name >> UpdateFilter idx)
    , input name idx op
    ]


intFilterInputs : Bool -> String -> Int -> Operation -> List (Html Msg)
intFilterInputs required name idx op =
    let
        options =
            List.map operationOption
                [ map Filter.equals int
                , map Filter.lesserThan int
                , map Filter.greaterThan int
                , map2 Filter.between int
                ]
    in
    [ select (options ++ nullOption required op) op
        |> Html.map (Filter.init name >> UpdateFilter idx)
    , input name idx op
    ]


floatFilterInputs : Bool -> String -> Int -> Operation -> List (Html Msg)
floatFilterInputs required name idx op =
    let
        options =
            List.map operationOption
                [ map Filter.equals float
                , map Filter.lesserThan float
                , map Filter.greaterThan float
                , map2 Filter.between float
                ]
    in
    [ select (options ++ nullOption required op) op
        |> Html.map (Filter.init name >> UpdateFilter idx)
    , input name idx op
    ]


dateFilterInputs : Bool -> String -> Int -> Operation -> List (Html Msg)
dateFilterInputs required name idx op =
    let
        options =
            List.map operationOption
                [ map Filter.inDate date
                , map Filter.lesserThan date
                , map Filter.greaterThan date
                , map2 Filter.between date
                ]
    in
    [ select (options ++ nullOption required op) op
        |> Html.map (Filter.init name >> UpdateFilter idx)
    , input name idx op
    ]


timeFilterInputs : Bool -> String -> Int -> Operation -> List (Html Msg)
timeFilterInputs required name idx op =
    let
        options =
            List.map operationOption
                [ map Filter.inDate date
                , map Filter.lesserThan time
                , map Filter.greaterThan time
                , map2 Filter.between time
                ]
    in
    [ select (options ++ nullOption required op) op
        |> Html.map (Filter.init name >> UpdateFilter idx)
    , input name idx op
    ]


boolFilterInputs : Bool -> String -> Int -> Operation -> List (Html Msg)
boolFilterInputs required name idx op =
    let
        options =
            List.map (dropBoth >> operationOption) [ IsTrue, IsFalse ]
    in
    [ select (options ++ nullOption required op) op
        |> Html.map (Filter.init name >> UpdateFilter idx)
    , input name idx op
    ]


enumInputs : Bool -> String -> Int -> Operation -> List (Html Msg)
enumInputs required name idx op =
    let
        options enum =
            List.map operationOption
                [ map OneOf (flip choose enum)
                , map NoneOf (flip choose enum)
                ]

        inputs makeOp enum =
            [ select (options enum ++ nullOption required op) op
                |> Html.map (Filter.init name >> UpdateFilter idx)
            , Operand.choices enum
                |> List.map (checkbox makeOp name idx enum)
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
                    [ select (options enum ++ nullOption required op_) op
                        |> Html.map (Filter.init name >> UpdateFilter idx)
                    ]

                NoneOf enum ->
                    [ select (options enum ++ nullOption required op_) op
                        |> Html.map (Filter.init name >> UpdateFilter idx)
                    ]

                _ ->
                    []

        _ ->
            []


choose : String -> Enum -> Enum
choose choice enum =
    let
        choices =
            Operand.choices enum

        chosen =
            Operand.chosen enum
    in
    if Set.member choice chosen then
        Operand.enum choices (Set.remove choice chosen)

    else if String.isEmpty choice then
        Operand.enum choices chosen

    else
        Operand.enum choices (Set.insert choice chosen)


select : Options -> Operation -> Html Operation
select options op =
    let
        opSelect a b =
            let
                makeOption ( s, _ ) =
                    option
                        [ selected (Operation.toString op == s) ]
                        [ Html.text s ]

                optionSelected selection =
                    let
                        makeOperation =
                            Dict.fromList options
                                |> Dict.get selection
                                |> Maybe.withDefault
                                    (dropBoth <| IsNull <| Just op)
                    in
                    makeOperation a b
            in
            Html.select
                [ onInput optionSelected ]
                (List.map makeOption options)
    in
    case Operation.values op of
        [ a, b ] ->
            opSelect a b

        [ a ] ->
            opSelect a ""

        _ ->
            opSelect "" ""


input : String -> Int -> Operation -> Html Msg
input name idx op =
    let
        input_ operationCons operand attrs =
            if text (Operand.value operand) == operand then
                textInput ([ type_ "text" ] ++ attrs)
                    (operationCons >> Filter.init name >> UpdateFilter idx)
                    operand

            else if int (Operand.value operand) == operand then
                textInput ([ type_ "number", step "1" ] ++ attrs)
                    (operationCons >> Filter.init name >> UpdateFilter idx)
                    operand

            else if float (Operand.value operand) == operand then
                textInput ([ type_ "number", step "0.01" ] ++ attrs)
                    (operationCons >> Filter.init name >> UpdateFilter idx)
                    operand

            else if date (Operand.value operand) == operand then
                textInput ([ type_ "date" ] ++ attrs)
                    (operationCons >> Filter.init name >> UpdateFilter idx)
                    operand

            else if date (Operand.value operand) == operand then
                textInput ([ type_ "datetime-local" ] ++ attrs)
                    (operationCons >> Filter.init name >> UpdateFilter idx)
                    operand

            else
                Html.text ""
    in
    case op of
        Equals a ->
            input_ Equals a []

        Contains a ->
            input_ Contains a []

        StartsWith a ->
            input_ StartsWith a []

        EndsWith a ->
            input_ EndsWith a []

        LesserThan a ->
            input_ LesserThan a []

        GreaterThan a ->
            input_ GreaterThan a []

        Between a b ->
            div []
                [ input_ (flip Between b) a []
                , span [] [ Html.text "and" ]
                , input_ (flip Between a) b []
                ]

        InDate a ->
            input_ InDate a [ type_ "date" ]

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


textInput : List (Attribute Msg) -> (Operand -> Msg) -> Operand -> Html Msg
textInput attributes makeOperation operand =
    Html.input
        ([ onInput (makeOperation << Operand.constructor operand)
         , onEnter Apply
         , value <| Operand.rawValue operand
         ]
            ++ attributes
        )
        []


checkbox : OperationConst -> String -> Int -> Enum -> String -> Html Msg
checkbox makeOp name idx enum choice =
    let
        inputId =
            String.fromInt idx |> (++) choice
    in
    label
        [ for inputId ]
        [ Html.input
            [ id inputId
            , value choice
            , onInput (\s -> makeOp s "" |> Filter.init name |> UpdateFilter idx)
            , Html.Attributes.type_ "checkbox"
            , checked <| Set.member choice (Operand.chosen enum)
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


onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg

            else
                Decode.fail "not ENTER"
    in
    on "keydown" (Decode.andThen isEnter keyCode)

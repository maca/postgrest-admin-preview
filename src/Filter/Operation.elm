module Filter.Operation exposing
    ( Operation(..)
    , boolFilterInputs
    , dateFilterInputs
    , enumInputs
    , floatFilterInputs
    , intFilterInputs
    , map
    , map2
    , textFilterInputs
    , timeFilterInputs
    , toPGQuery
    )

import Basics.Extra exposing (flip)
import Dict
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
import Html exposing (Attribute, Html, div, label, option, span)
import Html.Attributes
    exposing
        ( checked
        , class
        , for
        , id
        , selected
        , step
        , type_
        , value
        )
import Html.Events exposing (onInput)
import Postgrest.Client as PG
import Set
import String.Extra as String


type Operation
    = IsTrue
    | IsFalse
    | IsNull (Maybe Operation)
    | Equals Operand
    | LesserThan Operand
    | GreaterThan Operand
    | Between Operand Operand
    | Contains Operand
    | StartsWith Operand
    | EndsWith Operand
    | InDate Operand
    | OneOf Enum
    | NoneOf Enum


type alias OperationConst =
    String -> String -> Operation


type alias Options =
    List ( String, OperationConst )


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
                    option [ selected (toString op == s) ] [ Html.text s ]

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
    div
        []
        [ label
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
        ]


nullOption : Bool -> Operation -> List ( String, OperationConst )
nullOption required op =
    if required then
        []

    else
        [ operationOption <| dropBoth (IsNull <| Just op) ]


toPGQuery : String -> Operation -> Maybe PG.Param
toPGQuery name op =
    let
        param =
            PG.param name
    in
    case op of
        IsTrue ->
            Just <| param PG.true

        IsFalse ->
            Just <| param PG.false

        IsNull _ ->
            Just <| param PG.null

        Equals operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.map (param << PG.eq << PG.string)

        LesserThan operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.map (param << PG.lt << PG.string)

        GreaterThan operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.map (param << PG.gt << PG.string)

        Contains operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.map (\a -> param <| PG.ilike <| "*" ++ a ++ "*")

        StartsWith operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.map (\a -> param <| PG.ilike <| a ++ "*")

        EndsWith operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.map (\a -> param <| PG.ilike <| "*" ++ a)

        Between operandA operandB ->
            let
                makeOperation a b =
                    let
                        ( valA, valB ) =
                            case compare a b of
                                LT ->
                                    ( a, b )

                                _ ->
                                    ( b, a )
                    in
                    PG.and
                        [ PG.param name <| PG.gte <| PG.string valA
                        , PG.param name <| PG.lte <| PG.string valB
                        ]
            in
            Maybe.map2 makeOperation
                (String.nonEmpty <| Operand.value operandA)
                (String.nonEmpty <| Operand.value operandB)

        InDate operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.map (param << PG.eq << PG.string)

        OneOf (Enum _ chosen) ->
            if Set.isEmpty chosen then
                Just <| param PG.null

            else
                Just <| param <| PG.inList PG.string <| Set.toList chosen

        NoneOf (Enum choices chosen) ->
            if Set.isEmpty chosen then
                Just <| param <| PG.inList PG.string choices

            else
                Just <| param <| PG.not <| PG.inList PG.string <| Set.toList chosen


dropBoth : c -> a -> b -> c
dropBoth c _ _ =
    c


map : (op -> c) -> (s -> op) -> s -> s -> c
map cons mfun a _ =
    cons (mfun a)


map2 : (op -> op -> c) -> (s -> op) -> s -> s -> c
map2 cons mfun a b =
    cons (mfun a) (mfun b)


operationOption : OperationConst -> ( String, OperationConst )
operationOption cons =
    ( toString (cons "" ""), cons )


toString : Operation -> String
toString operation =
    case operation of
        IsTrue ->
            "is true"

        IsFalse ->
            "is false"

        IsNull _ ->
            "is not set"

        Equals _ ->
            "is equal to"

        LesserThan _ ->
            "is lesser than"

        GreaterThan _ ->
            "is greater than"

        Between _ _ ->
            "is between"

        Contains _ ->
            "contains"

        StartsWith _ ->
            "starts with"

        EndsWith _ ->
            "ends with"

        InDate _ ->
            "is in date"

        OneOf _ ->
            "is one of"

        NoneOf _ ->
            "is none of"

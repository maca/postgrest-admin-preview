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
import Set exposing (Set)
import String.Extra as String


type Operation
    = IsTrue
    | IsFalse
    | IsNull
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
    Maybe String -> Maybe String -> Operation


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
    [ select (options ++ nullOption required) op, input op ]


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
    [ select (options ++ nullOption required) op, input op ]


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
    [ select (options ++ nullOption required) op, input op ]


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
    [ select (options ++ nullOption required) op, input op ]


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
    [ select (options ++ nullOption required) op, input op ]


boolFilterInputs : Bool -> Operation -> List (Html Operation)
boolFilterInputs required op =
    let
        options =
            List.map (dropBoth >> operationOption) [ IsTrue, IsFalse ]
    in
    [ select (options ++ nullOption required) op, input op ]


enumInputs : Bool -> Int -> Operation -> List (Html Operation)
enumInputs required idx op =
    let
        options enum =
            List.map operationOption
                [ map OneOf (flip choose enum)
                , map NoneOf (flip choose enum)
                ]

        inputs makeOp ((Enum choices _) as enum) =
            [ select (options enum ++ nullOption required) op
            , choices
                |> List.map (checkbox makeOp idx enum)
                |> div [ class "checkboxes" ]
            ]
    in
    case op of
        OneOf ((Enum _ chosen) as enum) ->
            inputs (map OneOf (flip choose enum)) enum

        NoneOf ((Enum _ chosen) as enum) ->
            inputs (map NoneOf (flip choose enum)) enum

        _ ->
            [ Html.text "" ]


choose : Maybe String -> Enum -> Enum
choose mchoice (Enum choices chosen) =
    case mchoice of
        Just choice ->
            if Set.member choice chosen then
                Enum choices (Set.remove choice chosen)

            else
                Enum choices (Set.insert choice chosen)

        Nothing ->
            Enum choices chosen


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
                                |> Maybe.withDefault (dropBoth IsNull)
                    in
                    makeOperation a b
            in
            Html.select
                [ onInput optionSelected ]
                (List.map makeOption options)
    in
    case op of
        Equals a ->
            opSelect (Operand.value a) Nothing

        Contains a ->
            opSelect (Operand.value a) Nothing

        StartsWith a ->
            opSelect (Operand.value a) Nothing

        EndsWith a ->
            opSelect (Operand.value a) Nothing

        LesserThan a ->
            opSelect (Operand.value a) Nothing

        GreaterThan a ->
            opSelect (Operand.value a) Nothing

        Between a b ->
            opSelect (Operand.value a) (Operand.value b)

        InDate a ->
            opSelect (Operand.value a) Nothing

        OneOf chosen ->
            opSelect Nothing Nothing

        NoneOf chosen ->
            opSelect Nothing Nothing

        IsTrue ->
            opSelect Nothing Nothing

        IsFalse ->
            opSelect Nothing Nothing

        IsNull ->
            opSelect Nothing Nothing


input : Operation -> Html Operation
input op =
    let
        attributes operand =
            case operand of
                OText val ->
                    [ type_ "text" ]

                OInt val ->
                    [ type_ "number", step "1" ]

                OFloat val ->
                    [ type_ "number", step "0.01" ]

                ODate val ->
                    [ type_ "date" ]

                OTime val ->
                    [ type_ "datetime-local" ]

                NullOperand ->
                    []
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

        IsNull ->
            Html.text ""


textInput :
    List (Attribute Operation)
    -> (Operand -> Operation)
    -> Operand
    -> Html Operation
textInput attributes makeOperation operand =
    Html.input
        ([ onInput (makeOperation << Operand.constructor operand << Just)
         , value <| Maybe.withDefault "" <| Operand.rawValue operand
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
                , onInput (\s -> (makeOp <| Just s) Nothing)
                , Html.Attributes.type_ "checkbox"
                , checked <| Set.member choice chosen
                ]
                []
            , Html.text <| String.humanize choice
            ]
        ]


nullOption : Bool -> List ( String, OperationConst )
nullOption required =
    if required then
        []

    else
        [ operationOption <| dropBoth IsNull ]


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

        IsNull ->
            Just <| param PG.null

        Equals operand ->
            Operand.value operand
                |> Maybe.map (param << PG.eq << PG.string)

        LesserThan operand ->
            Operand.value operand
                |> Maybe.map (param << PG.lt << PG.string)

        GreaterThan operand ->
            Operand.value operand
                |> Maybe.map (param << PG.gt << PG.string)

        Contains operand ->
            Operand.value operand
                |> Maybe.map (\a -> param <| PG.ilike <| "*" ++ a ++ "*")

        StartsWith operand ->
            Operand.value operand
                |> Maybe.map (\a -> param <| PG.ilike <| a ++ "*")

        EndsWith operand ->
            Operand.value operand
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
                (Operand.value operandA)
                (Operand.value operandB)

        InDate operand ->
            Operand.value operand
                |> Maybe.map (param << PG.eq << PG.string)

        OneOf (Enum _ chosen) ->
            Just <| param <| PG.inList PG.string <| Set.toList chosen

        NoneOf (Enum _ chosen) ->
            Just <| param <| PG.not <| PG.inList PG.string <| Set.toList chosen


dropBoth : c -> a -> b -> c
dropBoth c _ _ =
    c


map : (op -> c) -> (ms -> op) -> ms -> ms -> c
map cons mfun a _ =
    cons (mfun a)


map2 : (op -> op -> c) -> (ms -> op) -> ms -> ms -> c
map2 cons mfun a b =
    cons (mfun a) (mfun b)


operationOption : OperationConst -> ( String, OperationConst )
operationOption cons =
    ( toString (cons Nothing Nothing), cons )


toString : Operation -> String
toString operation =
    case operation of
        IsTrue ->
            "is true"

        IsFalse ->
            "is false"

        IsNull ->
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

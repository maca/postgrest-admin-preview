module Filter.Operation exposing
    ( Operation(..)
    , boolFilterInputs
    , dateFilterInputs
    , enumInputs
    , floatFilterInputs
    , intFilterInputs
    , textFilterInputs
    , timeFilterInputs
    , toPGQuery
    )

import Basics.Extra exposing (flip)
import Dict
import Html exposing (Attribute, Html, div, label, option, span, text)
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
    | Equals (Maybe String)
    | LesserThan (Maybe String)
    | GreaterThan (Maybe String)
    | Between (Maybe String) (Maybe String)
    | Contains (Maybe String)
    | StartsWith (Maybe String)
    | EndsWith (Maybe String)
    | InDate (Maybe String)
    | OneOf (Set String)
    | NoneOf (Set String)


type alias OperationC =
    Maybe String -> Maybe String -> Operation


type alias Options =
    List ( String, OperationC )


textFilterInputs : Bool -> Operation -> List (Html Operation)
textFilterInputs required op =
    let
        options =
            [ ( "equals", dropLast Equals )
            , ( "contains", dropLast Contains )
            , ( "starts with", dropLast StartsWith )
            , ( "ends with", dropLast EndsWith )
            ]
    in
    inputs [ type_ "text" ] (options ++ nullOption required) op


intFilterInputs : Bool -> Operation -> List (Html Operation)
intFilterInputs required op =
    inputs [ type_ "number", step "1" ]
        (numberOptions ++ nullOption required)
        op


floatFilterInputs : Bool -> Operation -> List (Html Operation)
floatFilterInputs required op =
    inputs [ type_ "number", step "0.01" ]
        (numberOptions ++ nullOption required)
        op


dateFilterInputs : Bool -> Operation -> List (Html Operation)
dateFilterInputs required op =
    inputs [ type_ "date" ] (timeOptions ++ nullOption required) op


timeFilterInputs : Bool -> Operation -> List (Html Operation)
timeFilterInputs required op =
    inputs [ type_ "datetime-local" ] (timeOptions ++ nullOption required) op


enumInputs : Bool -> List String -> Int -> Operation -> List (Html Operation)
enumInputs required choices idx op =
    let
        select_ chosen =
            let
                options =
                    [ ( "is one of", dropLast <| enum OneOf chosen )
                    , ( "is none of", dropLast <| enum NoneOf chosen )
                    ]
            in
            select (options ++ nullOption required) op

        inputs_ makeOp chosen =
            [ select_ chosen
            , choices
                |> List.map (checkbox makeOp idx chosen)
                |> div [ class "checkboxes" ]
            ]
    in
    case op of
        OneOf chosen ->
            inputs_ (dropLast <| enum OneOf chosen) chosen

        NoneOf chosen ->
            inputs_ (dropLast <| enum NoneOf chosen) chosen

        _ ->
            [ select_ Set.empty ]


boolFilterInputs : Bool -> Operation -> List (Html Operation)
boolFilterInputs required op =
    inputs [] (boolOptions ++ nullOption required) op


inputs :
    List (Attribute Operation)
    -> Options
    -> Operation
    -> List (Html Operation)
inputs attributes options op =
    [ select options op, input attributes op ]


enum : (Set String -> Operation) -> Set String -> Maybe String -> Operation
enum makeEnum chosen mstring =
    case mstring of
        Just choice ->
            if Set.member choice chosen then
                makeEnum (Set.remove choice chosen)

            else
                makeEnum (Set.insert choice chosen)

        Nothing ->
            makeEnum chosen


select : Options -> Operation -> Html Operation
select options op =
    let
        opSelect makeOp a b =
            let
                makeOption ( s, f_ ) =
                    option
                        [ selected (makeOp a b == f_ a b) ]
                        [ text s ]
            in
            Html.select
                [ onInput (optionSelected options a b) ]
                (List.map makeOption options)
    in
    case op of
        Equals a ->
            opSelect (dropLast Equals) a Nothing

        Contains a ->
            opSelect (dropLast Contains) a Nothing

        StartsWith a ->
            opSelect (dropLast StartsWith) a Nothing

        EndsWith a ->
            opSelect (dropLast EndsWith) a Nothing

        LesserThan a ->
            opSelect (dropLast LesserThan) a Nothing

        GreaterThan a ->
            opSelect (dropLast GreaterThan) a Nothing

        Between a b ->
            opSelect Between a b

        InDate a ->
            opSelect (dropLast InDate) a Nothing

        OneOf chosen ->
            opSelect (dropLast <| enum OneOf chosen) Nothing Nothing

        NoneOf chosen ->
            opSelect (dropLast <| enum NoneOf chosen) Nothing Nothing

        IsTrue ->
            opSelect (dropBoth IsTrue) Nothing Nothing

        IsFalse ->
            opSelect (dropBoth IsFalse) Nothing Nothing

        IsNull ->
            opSelect (dropBoth IsNull) Nothing Nothing


optionSelected : Options -> Maybe String -> Maybe String -> String -> Operation
optionSelected options a b selection =
    let
        makeOp =
            Dict.fromList options
                |> Dict.get selection
                |> Maybe.withDefault (\_ _ -> IsNull)
    in
    makeOp a b


input : List (Attribute Operation) -> Operation -> Html Operation
input attributes op =
    case op of
        Equals a ->
            textInput attributes Equals a

        Contains a ->
            textInput attributes Contains a

        StartsWith a ->
            textInput attributes StartsWith a

        EndsWith a ->
            textInput attributes EndsWith a

        LesserThan a ->
            textInput attributes LesserThan a

        GreaterThan a ->
            textInput attributes GreaterThan a

        Between a b ->
            div []
                [ textInput attributes (flip Between b) a
                , span [] [ text "and" ]
                , textInput attributes (Between a) b
                ]

        InDate a ->
            textInput [ type_ "date" ] InDate a

        OneOf _ ->
            text ""

        NoneOf _ ->
            text ""

        IsTrue ->
            text ""

        IsFalse ->
            text ""

        IsNull ->
            text ""


textInput :
    List (Attribute Operation)
    -> (Maybe String -> Operation)
    -> Maybe String
    -> Html Operation
textInput attributes makeOp a =
    Html.input
        ([ onInput (Just >> makeOp), value <| Maybe.withDefault "" a ]
            ++ attributes
        )
        []


checkbox : OperationC -> Int -> Set String -> String -> Html Operation
checkbox makeOp idx chosen choice =
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
            , text <| String.humanize choice
            ]
        ]


nullOption : Bool -> List ( String, OperationC )
nullOption required =
    if required then
        []

    else
        [ ( "is not set", dropBoth IsNull ) ]


numberOptions : List ( String, OperationC )
numberOptions =
    [ ( "equals", dropLast Equals )
    , ( "is lesser than", dropLast LesserThan )
    , ( "is greater than", dropLast GreaterThan )
    , ( "is between", Between )
    ]


boolOptions : List ( String, OperationC )
boolOptions =
    [ ( "is true", dropBoth IsTrue )
    , ( "is false", dropBoth IsFalse )
    ]


timeOptions : List ( String, OperationC )
timeOptions =
    [ ( "is on date", dropLast InDate )
    , ( "is lesser than", dropLast LesserThan )
    , ( "is greater than", dropLast GreaterThan )
    , ( "is between", Between )
    ]


toPGQuery : String -> Operation -> Maybe PG.Param
toPGQuery name op =
    let
        param q =
            Just <| PG.param name q
    in
    case op of
        IsTrue ->
            param PG.true

        IsFalse ->
            param PG.false

        IsNull ->
            param PG.null

        Equals (Just a) ->
            param <| PG.eq <| PG.string a

        LesserThan (Just a) ->
            param <| PG.lt <| PG.string a

        GreaterThan (Just a) ->
            param <| PG.gt <| PG.string a

        Contains (Just a) ->
            param <| PG.ilike <| "*" ++ a ++ "*"

        StartsWith (Just a) ->
            param <| PG.ilike <| a ++ "*"

        EndsWith (Just a) ->
            param <| PG.ilike <| "*" ++ a

        Between (Just a) (Just b) ->
            let
                ( a_, b_ ) =
                    case compare a b of
                        LT ->
                            ( a, b )

                        _ ->
                            ( b, a )
            in
            Just <|
                PG.and
                    [ PG.param name <| PG.gte <| PG.string a_
                    , PG.param name <| PG.lte <| PG.string b_
                    ]

        InDate (Just a) ->
            param <| PG.eq <| PG.string a

        OneOf chosen ->
            param <| PG.inList PG.string <| Set.toList chosen

        NoneOf chosen ->
            param <| PG.not <| PG.inList PG.string <| Set.toList chosen

        _ ->
            Nothing


dropLast : (a -> c) -> a -> b -> c
dropLast fun a _ =
    fun a


dropBoth : c -> a -> b -> c
dropBoth c _ _ =
    c

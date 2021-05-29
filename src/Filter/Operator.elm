module Filter.Operator exposing
    ( Operator(..)
    , boolFilterInputs
    , dateFilterInputs
    , enumInputs
    , floatFilterInputs
    , intFilterInputs
    , textFilterInputs
    , timeFilterInputs
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


type Operator
    = Equals (Maybe String)
    | Contains (Maybe String)
    | StartsWith (Maybe String)
    | EndsWith (Maybe String)
    | LesserThan (Maybe String)
    | GreaterThan (Maybe String)
    | Between (Maybe String) (Maybe String)
    | InDate (Maybe String)
    | OneOf (List String) (Set String)
    | NoneOf (List String) (Set String)
    | IsTrue
    | IsFalse
    | IsNull


type alias OperatorC =
    Maybe String -> Maybe String -> Operator


type alias Options =
    List ( String, OperatorC )


textFilterInputs : Bool -> Operator -> List (Html Operator)
textFilterInputs required op =
    let
        options =
            [ ( "equals", equals )
            , ( "contains", contains )
            , ( "starts with", startsWith )
            , ( "ends with", endsWith )
            ]
    in
    inputs 0 [ type_ "text" ] (options ++ nullOption required) op


intFilterInputs : Bool -> Operator -> List (Html Operator)
intFilterInputs required op =
    inputs 0 [ type_ "number", step "1" ] (numberOptions ++ nullOption required) op


floatFilterInputs : Bool -> Operator -> List (Html Operator)
floatFilterInputs required op =
    inputs 0 [ type_ "number", step "0.01" ] (numberOptions ++ nullOption required) op


dateFilterInputs : Bool -> Operator -> List (Html Operator)
dateFilterInputs required op =
    inputs 0 [ type_ "date" ] (timeOptions ++ nullOption required) op


timeFilterInputs : Bool -> Operator -> List (Html Operator)
timeFilterInputs required op =
    inputs 0 [ type_ "datetime-local" ] (timeOptions ++ nullOption required) op


enumInputs : Bool -> Int -> Operator -> List (Html Operator)
enumInputs required idx op =
    let
        options =
            case op of
                OneOf choices chosen ->
                    [ ( "is one of", oneOf choices chosen )
                    , ( "is none of", noneOf choices chosen )
                    ]

                NoneOf choices chosen ->
                    [ ( "is one of", oneOf choices chosen )
                    , ( "is none of", noneOf choices chosen )
                    ]

                _ ->
                    []
    in
    inputs idx [] (timeOptions ++ nullOption required) op


boolFilterInputs : Bool -> Operator -> List (Html Operator)
boolFilterInputs required op =
    inputs 0 [] (boolOptions ++ nullOption required) op


inputs :
    Int
    -> List (Attribute Operator)
    -> Options
    -> Operator
    -> List (Html Operator)
inputs idx attributes options op =
    [ select options op, input idx attributes op ]


equals : OperatorC
equals a _ =
    Equals a


contains : OperatorC
contains a _ =
    Contains a


startsWith : OperatorC
startsWith a _ =
    StartsWith a


endsWith : OperatorC
endsWith a _ =
    EndsWith a


isTrue : OperatorC
isTrue _ _ =
    IsTrue


isFalse : OperatorC
isFalse _ _ =
    IsFalse


oneOf : List String -> Set String -> OperatorC
oneOf choices chosen mstring _ =
    let
        choice =
            Maybe.withDefault "" mstring
    in
    if Set.member choice chosen then
        OneOf choices (Set.remove choice chosen)

    else
        OneOf choices (Set.insert choice chosen)


noneOf : List String -> Set String -> OperatorC
noneOf choices chosen mstring _ =
    let
        choice =
            Maybe.withDefault "" mstring
    in
    if Set.member choice chosen then
        NoneOf choices (Set.remove choice chosen)

    else
        NoneOf choices (Set.insert choice chosen)


lesserThan : OperatorC
lesserThan a _ =
    LesserThan a


greaterThan : OperatorC
greaterThan a _ =
    GreaterThan a


between : OperatorC
between a b =
    Between a b


inDate : OperatorC
inDate a _ =
    InDate a


isNull : OperatorC
isNull _ _ =
    IsNull


select : Options -> Operator -> Html Operator
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
            opSelect equals a Nothing

        Contains a ->
            opSelect contains a Nothing

        StartsWith a ->
            opSelect startsWith a Nothing

        EndsWith a ->
            opSelect endsWith a Nothing

        LesserThan a ->
            opSelect lesserThan a Nothing

        GreaterThan a ->
            opSelect greaterThan a Nothing

        Between a b ->
            opSelect between a b

        InDate a ->
            opSelect inDate a Nothing

        OneOf choices chosen ->
            opSelect (oneOf choices chosen) Nothing Nothing

        NoneOf choices chosen ->
            opSelect (noneOf choices chosen) Nothing Nothing

        IsTrue ->
            opSelect isTrue Nothing Nothing

        IsFalse ->
            opSelect isFalse Nothing Nothing

        IsNull ->
            opSelect isNull Nothing Nothing


optionSelected :
    Options
    -> Maybe String
    -> Maybe String
    -> String
    -> Operator
optionSelected options a b selection =
    let
        makeOp =
            Dict.fromList options
                |> Dict.get selection
                |> Maybe.withDefault (\_ _ -> IsNull)
    in
    makeOp a b


input : Int -> List (Attribute Operator) -> Operator -> Html Operator
input idx attributes op =
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

        OneOf choices chosen ->
            choices
                |> List.map (checkbox (oneOf choices chosen) idx chosen)
                |> div [ class "checkboxes" ]

        NoneOf choices chosen ->
            choices
                |> List.map (checkbox (oneOf choices chosen) idx chosen)
                |> div [ class "checkboxes" ]

        IsTrue ->
            text ""

        IsFalse ->
            text ""

        IsNull ->
            text ""


textInput :
    List (Attribute Operator)
    -> (Maybe String -> Operator)
    -> Maybe String
    -> Html Operator
textInput attributes makeOp a =
    Html.input
        ([ onInput (Just >> makeOp), value <| Maybe.withDefault "" a ]
            ++ attributes
        )
        []


checkbox : OperatorC -> Int -> Set String -> String -> Html Operator
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


nullOption : Bool -> List ( String, OperatorC )
nullOption required =
    if required then
        []

    else
        [ ( "is not set", isNull ) ]


numberOptions : List ( String, OperatorC )
numberOptions =
    [ ( "equals", equals )
    , ( "is lesser than", lesserThan )
    , ( "is greater than", greaterThan )
    , ( "is between", between )
    ]


boolOptions : List ( String, OperatorC )
boolOptions =
    [ ( "is true", isTrue )
    , ( "is false", isFalse )
    ]


timeOptions : List ( String, OperatorC )
timeOptions =
    [ ( "is on date", inDate )
    , ( "is lesser than", lesserThan )
    , ( "is greater than", greaterThan )
    , ( "is between", between )
    ]

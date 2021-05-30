module Filter.Operator exposing
    ( Operator(..)
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


type Operator
    = Equals (Maybe String)
    | Contains (Maybe String)
    | StartsWith (Maybe String)
    | EndsWith (Maybe String)
    | LesserThan (Maybe String)
    | GreaterThan (Maybe String)
    | Between (Maybe String) (Maybe String)
    | InDate (Maybe String)
    | OneOf (Set String)
    | NoneOf (Set String)
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
    inputs [ type_ "text" ] (options ++ nullOption required) op


intFilterInputs : Bool -> Operator -> List (Html Operator)
intFilterInputs required op =
    inputs [ type_ "number", step "1" ]
        (numberOptions ++ nullOption required)
        op


floatFilterInputs : Bool -> Operator -> List (Html Operator)
floatFilterInputs required op =
    inputs [ type_ "number", step "0.01" ]
        (numberOptions ++ nullOption required)
        op


dateFilterInputs : Bool -> Operator -> List (Html Operator)
dateFilterInputs required op =
    inputs [ type_ "date" ] (timeOptions ++ nullOption required) op


timeFilterInputs : Bool -> Operator -> List (Html Operator)
timeFilterInputs required op =
    inputs [ type_ "datetime-local" ] (timeOptions ++ nullOption required) op


enumInputs : Bool -> List String -> Int -> Operator -> List (Html Operator)
enumInputs required choices idx op =
    let
        select_ chosen =
            let
                options =
                    [ ( "is one of", oneOf chosen )
                    , ( "is none of", noneOf chosen )
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
            inputs_ (oneOf chosen) chosen

        NoneOf chosen ->
            inputs_ (noneOf chosen) chosen

        _ ->
            [ select_ Set.empty ]


boolFilterInputs : Bool -> Operator -> List (Html Operator)
boolFilterInputs required op =
    inputs [] (boolOptions ++ nullOption required) op


inputs :
    List (Attribute Operator)
    -> Options
    -> Operator
    -> List (Html Operator)
inputs attributes options op =
    [ select options op, input attributes op ]


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


oneOf : Set String -> OperatorC
oneOf chosen mstring _ =
    enum OneOf chosen mstring


noneOf : Set String -> OperatorC
noneOf chosen mstring _ =
    enum NoneOf chosen mstring


enum : (Set String -> Operator) -> Set String -> Maybe String -> Operator
enum makeEnum chosen mstring =
    case mstring of
        Just choice ->
            if Set.member choice chosen then
                makeEnum (Set.remove choice chosen)

            else
                makeEnum (Set.insert choice chosen)

        Nothing ->
            makeEnum chosen


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

        OneOf chosen ->
            opSelect (oneOf chosen) Nothing Nothing

        NoneOf chosen ->
            opSelect (noneOf chosen) Nothing Nothing

        IsTrue ->
            opSelect isTrue Nothing Nothing

        IsFalse ->
            opSelect isFalse Nothing Nothing

        IsNull ->
            opSelect isNull Nothing Nothing


optionSelected : Options -> Maybe String -> Maybe String -> String -> Operator
optionSelected options a b selection =
    let
        makeOp =
            Dict.fromList options
                |> Dict.get selection
                |> Maybe.withDefault (\_ _ -> IsNull)
    in
    makeOp a b


input : List (Attribute Operator) -> Operator -> Html Operator
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


toPGQuery : String -> Operator -> Maybe PG.Param
toPGQuery name op =
    let
        param q =
            Just <| PG.param name q
    in
    case op of
        Equals (Just a) ->
            param <| PG.eq <| PG.string a

        Contains (Just a) ->
            param <| PG.ilike <| "*" ++ a ++ "*"

        StartsWith (Just a) ->
            param <| PG.ilike <| a ++ "*"

        EndsWith (Just a) ->
            param <| PG.ilike <| "*" ++ a

        LesserThan (Just a) ->
            param <| PG.lt <| PG.string a

        GreaterThan (Just a) ->
            param <| PG.gt <| PG.string a

        Between (Just a) (Just b) ->
            param <| PG.contains [ PG.string a, PG.string b ]

        InDate (Just a) ->
            param <| PG.eq <| PG.string a

        OneOf chosen ->
            param <| PG.inList PG.string <| Set.toList chosen

        NoneOf chosen ->
            param <| PG.not <| PG.inList PG.string <| Set.toList chosen

        IsTrue ->
            param PG.true

        IsFalse ->
            param PG.false

        IsNull ->
            param PG.null

        _ ->
            Nothing

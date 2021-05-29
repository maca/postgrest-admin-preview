module Filter.Time exposing (TimeInput(..), TimeOp, init, inputs)

import Basics.Extra exposing (flip)
import Dict
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (selected, type_, value)
import Html.Events exposing (onInput)


type TimeOp
    = TimeInDate (Maybe String)
    | TimeLesserThan (Maybe String)
    | TimeGreaterThan (Maybe String)
    | TimeBetween (Maybe String) (Maybe String)
    | IsNull


type TimeInput
    = DateInput
    | TimeInput


type alias OperationC =
    Maybe String -> Maybe String -> TimeOp


init : TimeOp
init =
    TimeInDate Nothing


inputs : TimeInput -> Bool -> TimeOp -> List (Html TimeOp)
inputs inputType required op =
    [ select required op, input inputType op ]


options : Bool -> List ( String, OperationC )
options required =
    [ ( "is on date", \s _ -> TimeInDate s )
    , ( "is lesser than", \s _ -> TimeLesserThan s )
    , ( "is greater than", \s _ -> TimeGreaterThan s )
    , ( "is between", TimeBetween )
    ]
        ++ (if required then
                []

            else
                [ ( "is not set", \_ _ -> IsNull ) ]
           )


optionSelected : Bool -> Maybe String -> Maybe String -> String -> TimeOp
optionSelected required a b selection =
    let
        makeOp =
            Dict.fromList (options required)
                |> Dict.get selection
                |> Maybe.withDefault (\s _ -> TimeInDate s)
    in
    makeOp a b


select : Bool -> TimeOp -> Html TimeOp
select required op =
    case op of
        TimeInDate a ->
            opSelect (\s _ -> TimeInDate s) required a Nothing

        TimeLesserThan a ->
            opSelect (\s _ -> TimeLesserThan s) required a Nothing

        TimeGreaterThan a ->
            opSelect (\s _ -> TimeGreaterThan s) required a Nothing

        TimeBetween a b ->
            opSelect TimeBetween required a b

        IsNull ->
            opSelect (\_ _ -> IsNull) required Nothing Nothing


opSelect : OperationC -> Bool -> Maybe String -> Maybe String -> Html TimeOp
opSelect makeOp required a b =
    let
        makeOption ( s, f_ ) =
            Html.option
                [ selected (makeOp a b == f_ a b) ]
                [ text s ]
    in
    Html.select
        [ onInput (optionSelected required a b) ]
        (List.map makeOption (options required))


input : TimeInput -> TimeOp -> Html TimeOp
input inputType op =
    case op of
        TimeInDate a ->
            timeInput DateInput TimeInDate a

        TimeLesserThan a ->
            timeInput inputType TimeLesserThan a

        TimeGreaterThan a ->
            timeInput inputType TimeGreaterThan a

        TimeBetween a b ->
            div []
                [ timeInput inputType (flip TimeBetween b) a
                , span [] [ text "and" ]
                , timeInput inputType (TimeBetween a) b
                ]

        IsNull ->
            text ""


timeInput : TimeInput -> (Maybe String -> TimeOp) -> Maybe String -> Html TimeOp
timeInput inputType makeOp mtime =
    Html.input
        [ type_ (inputTypeToString inputType)
        , onInput (Just >> makeOp)
        , value <| Maybe.withDefault "" mtime
        ]
        []


inputTypeToString : TimeInput -> String
inputTypeToString inputType =
    case inputType of
        DateInput ->
            "date"

        TimeInput ->
            "datetime-local"

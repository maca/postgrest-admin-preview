module Listing.Search.Time exposing (TimeInput(..), TimeOp(..), inputs)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, selected, type_, value)
import Html.Events exposing (onInput)
import Iso8601
import Time exposing (utc)


type TimeOp
    = TimeInDate (Maybe String)
    | TimeLesserThan (Maybe String)
    | TimeGreaterThan (Maybe String)
    | TimeBetween (Maybe String) (Maybe String)


type TimeInput
    = DateInput
    | TimeInput


type alias OperationC =
    Maybe String -> Maybe String -> TimeOp


inputs : TimeInput -> TimeOp -> List (Html TimeOp)
inputs inputType op =
    [ select op, input inputType op ]


options : List ( String, OperationC )
options =
    [ ( "is in date", \s _ -> TimeInDate s )
    , ( "is lesser than", \s _ -> TimeLesserThan s )
    , ( "is greater than", \s _ -> TimeGreaterThan s )
    , ( "is between", TimeBetween )
    ]


optionSelected : Maybe String -> Maybe String -> String -> TimeOp
optionSelected a b selection =
    let
        makeOp =
            Dict.fromList options
                |> Dict.get selection
                |> Maybe.withDefault (\s _ -> TimeInDate s)
    in
    makeOp a b


select : TimeOp -> Html TimeOp
select op =
    case op of
        TimeInDate a ->
            opSelect (\s _ -> TimeInDate s) a Nothing

        TimeLesserThan a ->
            opSelect (\s _ -> TimeLesserThan s) a Nothing

        TimeGreaterThan a ->
            opSelect (\s _ -> TimeGreaterThan s) a Nothing

        TimeBetween a b ->
            opSelect TimeBetween a b


opSelect : OperationC -> Maybe String -> Maybe String -> Html TimeOp
opSelect makeOp a b =
    let
        makeOption ( s, f_ ) =
            Html.option
                [ selected (makeOp a b == f_ a b) ]
                [ text s ]
    in
    Html.select
        [ onInput (optionSelected a b) ]
        (List.map makeOption options)


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

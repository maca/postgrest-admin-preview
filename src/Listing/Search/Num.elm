module Listing.Search.Num exposing (NumOp(..), inputs)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onInput)


type NumOp
    = NumEquals (Maybe Float)
    | NumLesserThan (Maybe Float)
    | NumGreaterThan (Maybe Float)
    | NumBetween (Maybe Float) (Maybe Float)


type alias OperationC =
    Maybe Float -> Maybe Float -> NumOp


inputs : NumOp -> List (Html NumOp)
inputs op =
    [ select op, input op ]


options : List ( String, OperationC )
options =
    [ ( "equals", \s _ -> NumEquals s )
    , ( "is lesser than", \s _ -> NumLesserThan s )
    , ( "is greater than", \s _ -> NumGreaterThan s )
    , ( "is between", NumBetween )
    ]


optionSelected : Maybe Float -> Maybe Float -> String -> NumOp
optionSelected a b selection =
    let
        makeOp =
            Dict.fromList options
                |> Dict.get selection
                |> Maybe.withDefault (\s _ -> NumEquals s)
    in
    makeOp a b


select : NumOp -> Html NumOp
select op =
    case op of
        NumEquals a ->
            opSelect (\s _ -> NumEquals s) a Nothing

        NumLesserThan a ->
            opSelect (\s _ -> NumLesserThan s) a Nothing

        NumGreaterThan a ->
            opSelect (\s _ -> NumGreaterThan s) a Nothing

        NumBetween a b ->
            opSelect NumBetween a b


opSelect : OperationC -> Maybe Float -> Maybe Float -> Html NumOp
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


input : NumOp -> Html NumOp
input op =
    case op of
        NumEquals a ->
            floatInput NumEquals a

        NumLesserThan a ->
            floatInput NumLesserThan a

        NumGreaterThan a ->
            floatInput NumGreaterThan a

        NumBetween a b ->
            div []
                [ floatInput (flip NumBetween b) a
                , span [] [ text "and" ]
                , floatInput (NumBetween a) b
                ]


floatInput : (Maybe Float -> NumOp) -> Maybe Float -> Html NumOp
floatInput makeOp mfloat =
    Html.input
        [ onInput (String.toFloat >> makeOp)
        , value <| Maybe.withDefault "" <| Maybe.map String.fromFloat mfloat
        ]
        []

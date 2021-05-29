module Listing.Search.Num exposing (NumInput(..), NumOp(..), inputs)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, selected, step, type_, value)
import Html.Events exposing (onInput)


type NumOp
    = NumEquals (Maybe String)
    | NumLesserThan (Maybe String)
    | NumGreaterThan (Maybe String)
    | NumBetween (Maybe String) (Maybe String)


type NumInput
    = FloatInput
    | IntInput


type alias OperationC =
    Maybe String -> Maybe String -> NumOp


inputs : NumInput -> NumOp -> List (Html NumOp)
inputs inputType op =
    [ select op, input inputType op ]


options : List ( String, OperationC )
options =
    [ ( "equals", \s _ -> NumEquals s )
    , ( "is lesser than", \s _ -> NumLesserThan s )
    , ( "is greater than", \s _ -> NumGreaterThan s )
    , ( "is between", NumBetween )
    ]


optionSelected : Maybe String -> Maybe String -> String -> NumOp
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


opSelect : OperationC -> Maybe String -> Maybe String -> Html NumOp
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


input : NumInput -> NumOp -> Html NumOp
input inputType op =
    case op of
        NumEquals a ->
            floatInput inputType NumEquals a

        NumLesserThan a ->
            floatInput inputType NumLesserThan a

        NumGreaterThan a ->
            floatInput inputType NumGreaterThan a

        NumBetween a b ->
            div []
                [ floatInput inputType (flip NumBetween b) a
                , span [] [ text "and" ]
                , floatInput inputType (NumBetween a) b
                ]


floatInput : NumInput -> (Maybe String -> NumOp) -> Maybe String -> Html NumOp
floatInput inputType makeOp mfloat =
    let
        step_ =
            case inputType of
                FloatInput ->
                    step "0.01"

                IntInput ->
                    step "1"
    in
    Html.input
        [ type_ "number"
        , step_
        , onInput (Just >> makeOp)
        , value <| Maybe.withDefault "" mfloat
        ]
        []

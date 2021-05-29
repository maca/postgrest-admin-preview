module Listing.Search.Enum exposing (EnumOp(..), inputs)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Html exposing (Html, div, label, option, span, text)
import Html.Attributes exposing (checked, for, id, selected, type_, value)
import Html.Events exposing (onInput)
import Set exposing (Set)
import String.Extra as String


type EnumOp
    = OneOf (Set String) (Set String)
    | NoneOf (Set String) (Set String)


type alias OperationC =
    Set String -> Set String -> EnumOp


inputs : EnumOp -> List (Html EnumOp)
inputs op =
    [ select op, checkboxes op ]


options : List ( String, OperationC )
options =
    [ ( "is one of", OneOf )
    , ( "is none of", NoneOf )
    ]


optionSelected : Set String -> Set String -> String -> EnumOp
optionSelected choices chosen selection =
    let
        makeOp =
            Dict.fromList options
                |> Dict.get selection
                |> Maybe.withDefault OneOf
    in
    makeOp choices chosen


select : EnumOp -> Html EnumOp
select op =
    case op of
        OneOf choices chosen ->
            operationSelect OneOf choices chosen

        NoneOf choices chosen ->
            operationSelect NoneOf choices chosen


operationSelect : OperationC -> Set String -> Set String -> Html EnumOp
operationSelect makeOp choices chosen =
    let
        makeOption ( s, f_ ) =
            option
                [ selected (makeOp choices chosen == f_ choices chosen) ]
                [ text s ]
    in
    Html.select
        [ onInput (optionSelected choices chosen) ]
        (List.map makeOption options)


checkboxes : EnumOp -> Html EnumOp
checkboxes op =
    case op of
        OneOf choices chosen ->
            enumCheckboxes OneOf choices chosen

        NoneOf choices chosen ->
            enumCheckboxes NoneOf choices chosen


enumCheckboxes : OperationC -> Set String -> Set String -> Html EnumOp
enumCheckboxes makeOp choices chosen =
    Set.toList choices
        |> List.map (checkbox (makeOp choices) chosen)
        |> div []


checkbox : (Set String -> EnumOp) -> Set String -> String -> Html EnumOp
checkbox makeOp chosen choice =
    div
        []
        [ label
            [ for choice ]
            [ Html.input
                [ id choice
                , value choice
                , onInput
                    (\s ->
                        if Set.member s chosen then
                            makeOp <| Set.remove s chosen

                        else
                            makeOp <| Set.insert s chosen
                    )
                , Html.Attributes.type_ "checkbox"
                , checked <| Set.member choice chosen
                ]
                []
            , text <| String.humanize choice
            ]
        ]

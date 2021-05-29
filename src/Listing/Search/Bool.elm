module Listing.Search.Bool exposing (BoolOp(..), init, input)

import Html exposing (Html, div, option, span, text)
import Html.Attributes exposing (selected)
import Html.Events exposing (onInput)


type BoolOp
    = BoolTrue
    | BoolFalse
    | IsNull


init : BoolOp
init =
    BoolTrue


input : Bool -> BoolOp -> Html BoolOp
input required val =
    div []
        [ span [] [ text "is" ]
        , Html.select
            [ onInput optSelected ]
            ([ option [ selected (val == BoolTrue) ] [ text "true" ]
             , option [ selected (val == BoolFalse) ] [ text "false" ]
             ]
                ++ (if required then
                        []

                    else
                        [ option [ selected (val == IsNull) ] [ text "is not set" ] ]
                   )
            )
        ]


optSelected : String -> BoolOp
optSelected selection =
    case selection of
        "true" ->
            BoolTrue

        "false" ->
            BoolFalse

        _ ->
            IsNull

module Listing.Search.Bool exposing (BoolOp(..), input)

import Html exposing (Html, div, option, span, text)
import Html.Attributes exposing (selected)
import Html.Events exposing (onInput)


type BoolOp
    = BoolOp Bool


type alias Options =
    List ( String, Maybe String -> BoolOp )


input : BoolOp -> Html BoolOp
input (BoolOp val) =
    div []
        [ span [] [ text "is" ]
        , Html.select
            [ onInput ((==) "true" >> BoolOp) ]
            [ option [ selected val ] [ text "true" ]
            , option [ selected (not val) ] [ text "false" ]
            ]
        ]

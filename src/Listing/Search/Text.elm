module Listing.Search.Text exposing (TextOp(..), input, select)

import Dict exposing (Dict)
import Html exposing (Html, option, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onInput)


type TextOp
    = TextEquals (Maybe String)
    | TextContains (Maybe String)
    | TextStartsWith (Maybe String)
    | TextEndsWith (Maybe String)


type alias Options =
    List ( String, Maybe String -> TextOp )


options : Options
options =
    [ ( "equals", TextEquals )
    , ( "contains", TextContains )
    , ( "starts with", TextStartsWith )
    , ( "ends with", TextEndsWith )
    ]


optionSelected : Maybe String -> String -> TextOp
optionSelected mstring selection =
    let
        makeOp =
            Dict.fromList options
                |> Dict.get selection
                |> Maybe.withDefault TextEquals
    in
    makeOp mstring


select : TextOp -> Html TextOp
select op =
    let
        opSelect makeOp mstring =
            let
                makeOption ( s, f_ ) =
                    option
                        [ selected (makeOp mstring == f_ mstring) ]
                        [ text s ]
            in
            Html.select
                [ onInput (optionSelected mstring) ]
                (List.map makeOption options)
    in
    case op of
        TextEquals mstring ->
            opSelect TextEquals mstring

        TextContains mstring ->
            opSelect TextContains mstring

        TextStartsWith mstring ->
            opSelect TextStartsWith mstring

        TextEndsWith mstring ->
            opSelect TextEndsWith mstring


input : TextOp -> Html TextOp
input op =
    let
        textInput makeOp mstring =
            Html.input
                [ onInput (Just >> makeOp)
                , value <| Maybe.withDefault "" mstring
                ]
                []
    in
    case op of
        TextEquals mstring ->
            textInput TextEquals mstring

        TextContains mstring ->
            textInput TextContains mstring

        TextStartsWith mstring ->
            textInput TextStartsWith mstring

        TextEndsWith mstring ->
            textInput TextEndsWith mstring

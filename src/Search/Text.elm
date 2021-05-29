module Search.Text exposing (TextOp(..), init, inputs)

import Dict
import Html exposing (Html, option, text)
import Html.Attributes exposing (selected, type_, value)
import Html.Events exposing (onInput)


type TextOp
    = TextEquals (Maybe String)
    | TextContains (Maybe String)
    | TextStartsWith (Maybe String)
    | TextEndsWith (Maybe String)
    | IsNull


init : TextOp
init =
    TextEquals Nothing


inputs : Bool -> TextOp -> List (Html TextOp)
inputs required op =
    [ select required op, input op ]


options : Bool -> List ( String, Maybe String -> TextOp )
options required =
    [ ( "equals", TextEquals )
    , ( "contains", TextContains )
    , ( "starts with", TextStartsWith )
    , ( "ends with", TextEndsWith )
    ]
        ++ (if required then
                []

            else
                [ ( "is not set", always IsNull ) ]
           )


optionSelected : Bool -> Maybe String -> String -> TextOp
optionSelected required mstring selection =
    let
        makeOp =
            Dict.fromList (options required)
                |> Dict.get selection
                |> Maybe.withDefault TextEquals
    in
    makeOp mstring


select : Bool -> TextOp -> Html TextOp
select required op =
    let
        opSelect makeOp mstring =
            let
                makeOption ( s, f_ ) =
                    option
                        [ selected (makeOp mstring == f_ mstring) ]
                        [ text s ]
            in
            Html.select
                [ onInput (optionSelected required mstring) ]
                (List.map makeOption (options required))
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

        IsNull ->
            opSelect (always IsNull) Nothing


input : TextOp -> Html TextOp
input op =
    let
        textInput makeOp mstring =
            Html.input
                [ onInput (Just >> makeOp)
                , type_ "text"
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

        IsNull ->
            text ""

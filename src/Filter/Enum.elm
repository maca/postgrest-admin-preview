module Filter.Enum exposing (EnumOp, init, inputs)

import Dict
import Html exposing (Html, div, label, option, text)
import Html.Attributes
    exposing
        ( checked
        , class
        , for
        , id
        , selected
        , type_
        , value
        )
import Html.Events exposing (onInput)
import Set exposing (Set)
import String.Extra as String


type EnumOp
    = OneOf (List String) (Set String)
    | NoneOf (List String) (Set String)
    | IsNull (List String)


type alias OperationC =
    List String -> Set String -> EnumOp


init : List String -> EnumOp
init choices =
    OneOf choices Set.empty


inputs : Bool -> EnumOp -> Int -> List (Html EnumOp)
inputs required op idx =
    [ select required op, checkboxes op idx ]


options : Bool -> List ( String, OperationC )
options required =
    [ ( "is one of", OneOf )
    , ( "is none of", NoneOf )
    ]
        ++ (if required then
                []

            else
                [ ( "is not set", \choices _ -> IsNull choices ) ]
           )


optionSelected : Bool -> List String -> Set String -> String -> EnumOp
optionSelected required choices chosen selection =
    let
        makeOp =
            Dict.fromList (options required)
                |> Dict.get selection
                |> Maybe.withDefault OneOf
    in
    makeOp choices chosen


select : Bool -> EnumOp -> Html EnumOp
select required op =
    case op of
        OneOf choices chosen ->
            operationSelect OneOf required choices chosen

        NoneOf choices chosen ->
            operationSelect NoneOf required choices chosen

        IsNull choices ->
            operationSelect (\_ _ -> IsNull choices) required choices Set.empty


operationSelect : OperationC -> Bool -> List String -> Set String -> Html EnumOp
operationSelect makeOp required choices chosen =
    let
        makeOption ( s, f_ ) =
            option
                [ selected (makeOp choices chosen == f_ choices chosen) ]
                [ text s ]
    in
    Html.select
        [ onInput (optionSelected required choices chosen) ]
        (List.map makeOption (options required))


checkboxes : EnumOp -> Int -> Html EnumOp
checkboxes op idx =
    case op of
        OneOf choices chosen ->
            enumCheckboxes OneOf idx choices chosen

        NoneOf choices chosen ->
            enumCheckboxes NoneOf idx choices chosen

        IsNull _ ->
            text ""


enumCheckboxes : OperationC -> Int -> List String -> Set String -> Html EnumOp
enumCheckboxes makeOp idx choices chosen =
    choices
        |> List.map (checkbox (makeOp choices) idx chosen)
        |> div [ class "checkboxes" ]


checkbox : (Set String -> EnumOp) -> Int -> Set String -> String -> Html EnumOp
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

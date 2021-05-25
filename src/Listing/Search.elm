module Listing.Search exposing (Msg, Search, init, update, view)

import Array exposing (Array)
import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Html
    exposing
        ( Html
        , button
        , div
        , form
        , h3
        , h4
        , i
        , input
        , option
        , select
        , text
        )
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onClick, onInput)
import Postgrest.Schema.Definition exposing (Column(..), Definition)
import Postgrest.Value as Value exposing (Value(..))
import String.Extra as String


type TextOp
    = TextEquals (Maybe String)
    | TextContains (Maybe String)
    | TextStartsWith (Maybe String)
    | TextEndsWith (Maybe String)


type EnumOp
    = EnumAll
    | EnumSelect (List String)


type NumOp
    = NumEquals String
    | NumBetween String String
    | NumGreaterThan String
    | NumLesserThan String


type BoolOp
    = BoolTrue
    | BoolFalse


type DateOp
    = DateEquals String
    | DateBetween String String
    | DateGreaterThan String
    | DateLesserThan String


type TimeOp
    = TimeBetween String String
    | TimeGreaterThan String
    | TimeLesserThan String


type Filter
    = TextFilter String TextOp
    | BoolFilter String BoolOp
    | NumFilter String NumOp
    | EnumFilter String EnumOp
    | DateFilter String DateOp
    | TimeFilter String TimeOp
    | Blank


type Msg
    = UpdateFilter Int Filter
    | AddFilter


type alias Search =
    { definition : Definition
    , filters : Array Filter
    }


init : Definition -> Search
init definition =
    { definition = definition
    , filters = Array.empty
    }


update : Msg -> Search -> ( Search, Cmd Msg )
update msg search =
    case msg of
        UpdateFilter idx filter ->
            ( { search | filters = Array.set idx filter search.filters }
            , Cmd.none
            )

        AddFilter ->
            let
                filter =
                    search.definition
                        |> Dict.toList
                        |> List.head
                        |> Maybe.map (\( n, c ) -> fromColumn Nothing n c)
                        |> Maybe.withDefault Blank
            in
            ( { search | filters = Array.push filter search.filters }
            , Cmd.none
            )


view : Search -> Html Msg
view { definition, filters } =
    div
        []
        ([ h3 [] [ text "filter" ] ]
            ++ (Array.indexedMap (viewFilter definition) filters |> Array.toList)
            ++ [ button
                    [ onClick AddFilter ]
                    [ i [ class "icono-plus" ] [] ]
               ]
        )


viewFilter definition idx filter =
    let
        fieldSelect name makeF =
            select
                [ onInput (makeF >> UpdateFilter idx) ]
                (Dict.keys definition
                    |> List.map
                        (\s ->
                            option
                                [ selected (s == name), value s ]
                                [ text <| String.humanize s ]
                        )
                )
    in
    case filter of
        Blank ->
            text ""

        TextFilter name op ->
            let
                makeFilter =
                    TextFilter name >> UpdateFilter idx

                opts =
                    [ ( "equals", TextEquals )
                    , ( "contains", TextContains )
                    , ( "starts with", TextStartsWith )
                    , ( "ends with", TextEndsWith )
                    ]

                optsDict =
                    Dict.fromList opts

                makeSelect f mstring =
                    let
                        makeOption ( s, f_ ) =
                            option
                                [ selected (f mstring == f_ mstring) ]
                                [ text s ]
                    in
                    select
                        [ onInput <|
                            \s ->
                                let
                                    makeOp =
                                        Dict.get s optsDict
                                            |> Maybe.withDefault TextEquals
                                in
                                makeOp mstring |> makeFilter
                        ]
                    <|
                        List.map makeOption opts

                makeInput makeOp mstring =
                    input
                        [ onInput (Just >> makeOp >> makeFilter)
                        , value <| Maybe.withDefault "" mstring
                        ]
                        []

                inputs makeOp mstring =
                    let
                        makeF k =
                            let
                                f =
                                    Dict.get k definition
                                        |> Maybe.map (fromColumn mstring name)
                                        |> Maybe.withDefault Blank
                            in
                            case f of
                                TextFilter _ _ ->
                                    TextFilter k <| makeOp mstring

                                _ ->
                                    f
                    in
                    div [ class "text filter" ]
                        [ fieldSelect name makeF
                        , makeSelect makeOp mstring
                        , makeInput makeOp mstring
                        ]
            in
            case op of
                TextEquals mstring ->
                    inputs TextEquals mstring

                TextContains mstring ->
                    inputs TextContains mstring

                TextStartsWith mstring ->
                    inputs TextStartsWith mstring

                TextEndsWith mstring ->
                    inputs TextEndsWith mstring

        _ ->
            text ""


fromColumn : Maybe String -> String -> Column -> Filter
fromColumn mdefault name (Column _ value) =
    let
        default =
            mdefault
                |> Maybe.andThen
                    (flip Value.updateWithString value >> Value.toString)
    in
    case value of
        PString _ ->
            TextFilter name <| TextEquals default

        PText _ ->
            TextFilter name <| TextEquals default

        PEnum _ _ ->
            EnumFilter name EnumAll

        PFloat _ ->
            Blank

        PInt _ ->
            Blank

        PBool _ ->
            Blank

        PTime _ ->
            Blank

        PDate _ ->
            Blank

        PPrimaryKey mprimaryKey ->
            Blank

        PForeignKey mprimaryKey { label } ->
            Blank

        BadValue _ ->
            Blank

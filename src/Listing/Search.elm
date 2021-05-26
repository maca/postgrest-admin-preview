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


type NumOp
    = NumEquals (Maybe Float)
    | NumBetween (Maybe Float) (Maybe Float)
    | NumGreaterThan (Maybe Float)
    | NumLesserThan (Maybe Float)


type BoolOp
    = BoolTrue
    | BoolFalse


type EnumOp
    = EnumAll
    | EnumSelect (List String)


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
    | NumFilter String NumOp
    | BoolFilter String BoolOp
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
                        |> Maybe.map (\( n, c ) -> fromColumn n c)
                        |> Maybe.withDefault Blank
            in
            ( { search | filters = Array.push filter search.filters }
            , Cmd.none
            )


view : Search -> Html Msg
view { definition, filters } =
    let
        _ =
            Debug.log "filters" filters
    in
    div
        []
        ([ h3 [] [ text "filter" ] ]
            ++ (Array.indexedMap (viewFilter definition) filters |> Array.toList)
            ++ [ button [ onClick AddFilter ] [ i [ class "icono-plus" ] [] ] ]
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
                updateMsg =
                    TextFilter name >> UpdateFilter idx

                operationSelect makeOp mstring options =
                    let
                        makeOption ( s, f_ ) =
                            option
                                [ selected (makeOp mstring == f_ mstring) ]
                                [ text s ]

                        inputMsg userSelection =
                            case Dict.get userSelection options of
                                Just f ->
                                    updateMsg <| f mstring

                                Nothing ->
                                    updateMsg <| TextEquals mstring
                    in
                    select [ onInput inputMsg ] <| List.map makeOption textFilterOpts

                filterInputs makeOp mstring =
                    let
                        makeFilter selection =
                            case defaultFilter selection definition of
                                TextFilter _ _ ->
                                    TextFilter selection <| makeOp mstring

                                _ ->
                                    defaultFilter selection definition
                    in
                    div [ class "text filter" ]
                        [ fieldSelect name makeFilter
                        , operationSelect makeOp mstring <|
                            Dict.fromList textFilterOpts
                        , filterInput (Just >> makeOp >> updateMsg) mstring
                        ]
            in
            case op of
                TextEquals mstring ->
                    filterInputs TextEquals mstring

                TextContains mstring ->
                    filterInputs TextContains mstring

                TextStartsWith mstring ->
                    filterInputs TextStartsWith mstring

                TextEndsWith mstring ->
                    filterInputs TextEndsWith mstring

        _ ->
            text ""


filterInput : (String -> Msg) -> Maybe String -> Html Msg
filterInput tagger mstring =
    input
        [ onInput tagger
        , value <| Maybe.withDefault "" mstring
        ]
        []


defaultFilter : String -> Definition -> Filter
defaultFilter colName definition =
    Dict.get colName definition
        |> Maybe.map (fromColumn colName)
        |> Maybe.withDefault Blank


textFilterOpts : List ( String, Maybe String -> TextOp )
textFilterOpts =
    [ ( "equals", TextEquals )
    , ( "contains", TextContains )
    , ( "starts with", TextStartsWith )
    , ( "ends with", TextEndsWith )
    ]


fromColumn : String -> Column -> Filter
fromColumn name (Column _ value) =
    case value of
        PString _ ->
            TextFilter name <| TextEquals Nothing

        PText _ ->
            TextFilter name <| TextEquals Nothing

        PFloat _ ->
            NumFilter name <| NumEquals Nothing

        PInt _ ->
            NumFilter name <| NumEquals Nothing

        PBool _ ->
            Blank

        PEnum _ _ ->
            EnumFilter name EnumAll

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

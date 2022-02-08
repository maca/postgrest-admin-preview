module Filter exposing
    ( Filter
    , between
    , columnName
    , contains
    , date
    , date2
    , endsWith
    , equals
    , float
    , float2
    , fromColumn
    , greaterOrEqual
    , greaterThan
    , inDate
    , init
    , int
    , int2
    , isFalse
    , isInTheFuture
    , isInThePast
    , isNull
    , isTrue
    , lesserOrEqual
    , lesserThan
    , noneOf
    , oneOf
    , operation
    , parse
    , startsWith
    , text
    , time
    , time2
    , toPGQuery
    , toQueryString
    )

import Basics.Extra exposing (flip)
import Dict
import Filter.Operand as Operand exposing (Operand(..))
import Filter.Operation as Operation exposing (Operation(..))
import Filter.Parser
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Trailing(..)
        , succeed
        , symbol
        , token
        )
import Postgrest.Client as PG
import Postgrest.Schema.Table exposing (Column, Table, columnValue)
import Postgrest.Value exposing (Value(..))
import Set exposing (Set)
import Time exposing (posixToMillis, toHour, utc)
import Time.Extra as Time


type Filter
    = Filter String Operation


type alias OperandConst =
    String -> Operand


toPGQuery : Filter -> Maybe PG.Param
toPGQuery (Filter name op) =
    Operation.toPGQuery name op


toQueryString : Filter -> String
toQueryString (Filter name op) =
    Operation.toPGQuery name op
        |> List.singleton
        |> List.filterMap identity
        |> PG.toQueryString


columnName : Filter -> String
columnName (Filter name _) =
    name


operation : Filter -> Operation
operation (Filter _ op) =
    op



-- Constructors


fromColumn : String -> Column -> Maybe Filter
fromColumn name col =
    case columnValue col of
        PString _ ->
            Just <| text name equals ""

        PText _ ->
            Just <| text name equals ""

        PInt _ ->
            Just <| int name equals ""

        PFloat _ ->
            Just <| float name equals ""

        PBool _ ->
            Just <| isTrue name

        PTime _ ->
            Just <| time name inDate ""

        PDate _ ->
            Just <| date name inDate ""

        PEnum _ choices ->
            Just <| oneOf name choices Set.empty

        PPrimaryKey _ ->
            Nothing

        PForeignKey _ _ ->
            Nothing

        Unknown _ ->
            Nothing


init : String -> Operation -> Filter
init =
    Filter


oneOf : String -> List String -> Set String -> Filter
oneOf name choices chosen =
    Filter name <| OneOf <| Operand.enum choices chosen


noneOf : String -> List String -> Set String -> Filter
noneOf name choices chosen =
    Filter name <| NoneOf <| Operand.enum choices chosen



-- Operation constructors


equals : Operand -> Operation
equals =
    Equals


lesserOrEqual : Operand -> Operation
lesserOrEqual =
    LesserOrEqual


greaterOrEqual : Operand -> Operation
greaterOrEqual =
    GreaterOrEqual


lesserThan : Operand -> Operation
lesserThan =
    LesserThan


greaterThan : Operand -> Operation
greaterThan =
    GreaterThan


between : Operand -> Operand -> Operation
between =
    Between


contains : Operand -> Operation
contains =
    Contains


startsWith : Operand -> Operation
startsWith =
    StartsWith


endsWith : Operand -> Operation
endsWith =
    EndsWith


inDate : Operand -> Operation
inDate =
    InDate



-- Filter constructors


text : String -> (Operand -> Operation) -> String -> Filter
text name operationCons value =
    init name (operationCons <| Operand.text value)


int : String -> (Operand -> Operation) -> String -> Filter
int name operationCons value =
    init name (operationCons <| Operand.int value)


int2 : String -> (Operand -> Operand -> Operation) -> String -> String -> Filter
int2 name operationCons valueA valueB =
    init name (operationCons (Operand.int valueA) (Operand.int valueB))


float : String -> (Operand -> Operation) -> String -> Filter
float name operationCons value =
    init name (operationCons <| Operand.float value)


float2 : String -> (Operand -> Operand -> Operation) -> String -> String -> Filter
float2 name operationCons valueA valueB =
    init name (operationCons (Operand.float valueA) (Operand.float valueB))


date : String -> (Operand -> Operation) -> String -> Filter
date name operationCons value =
    init name (operationCons <| Operand.date value)


date2 : String -> (Operand -> Operand -> Operation) -> String -> String -> Filter
date2 name operationCons valueA valueB =
    init name (operationCons (Operand.date valueA) (Operand.date valueB))


time : String -> (Operand -> Operation) -> String -> Filter
time name operationCons value =
    init name (operationCons <| Operand.time value)


time2 : String -> (Operand -> Operand -> Operation) -> String -> String -> Filter
time2 name operationCons valueA valueB =
    init name (operationCons (Operand.time valueA) (Operand.time valueB))


isTrue : String -> Filter
isTrue name =
    Filter name IsTrue


isFalse : String -> Filter
isFalse name =
    Filter name IsFalse


isNull : String -> Filter
isNull name =
    Filter name (IsNull Nothing)


isInTheFuture : String -> Filter
isInTheFuture name =
    Filter name (IsInTheFuture Nothing)


isInThePast : String -> Filter
isInThePast name =
    Filter name (IsInThePast Nothing)



-- Parsing


parse : Table -> String -> Maybe Filter
parse table query =
    query
        |> Parser.run (parseFilter table)
        |> Result.toMaybe
        |> Maybe.andThen identity


parseFilter : Table -> Parser (Maybe Filter)
parseFilter table =
    Parser.oneOf
        [ and table
        , columnFilter table "="
        ]



-- Parse helpers


and : Table -> Parser (Maybe Filter)
and table =
    succeed combineAnd
        |. symbol "and="
        |= list table


list : Table -> Parser (List (Maybe Filter))
list table =
    Parser.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = Parser.spaces
        , item = columnFilter table "."
        , trailing = Forbidden
        }


columnFilter : Table -> String -> Parser (Maybe Filter)
columnFilter table separator =
    let
        colNames =
            Dict.keys table
                |> List.map (\s -> succeed (always s) |= token s)
    in
    succeed (\name f -> f name)
        |= Parser.oneOf colNames
        |. symbol separator
        |= Parser.oneOf
            [ succeed (enumCons table)
                |= Filter.Parser.enum
            , succeed (filterCons table)
                |= Filter.Parser.operation
            ]


combineAnd : List (Maybe Filter) -> Maybe Filter
combineAnd filters =
    combine [] filters |> List.head


combine : List Filter -> List (Maybe Filter) -> List Filter
combine acc filters =
    case filters of
        [] ->
            acc

        Nothing :: fs ->
            combine acc fs

        (Just f) :: fs ->
            let
                ( rest, combined ) =
                    fs
                        |> List.foldl
                            (\mf ( rem, comb ) ->
                                case mf of
                                    Just f_ ->
                                        combineHelp f f_
                                            |> Tuple.mapFirst (flip (::) rem)
                                            |> Tuple.mapSecond (flip (::) comb)

                                    Nothing ->
                                        ( rem, comb )
                            )
                            ( [], [] )
            in
            combine (List.filterMap identity combined ++ acc) rest


combineHelp : Filter -> Filter -> ( Maybe Filter, Maybe Filter )
combineHelp (Filter name op) ((Filter name_ op_) as f_) =
    let
        default =
            ( Just f_, Nothing )

        inDateMapFn min max =
            let
                diffIs24H =
                    posixToMillis max - posixToMillis min == Time.hours24
            in
            diffIs24H && toHour utc min == 0

        makeInDate min max =
            case ( min, max ) of
                ( Operand.Time tmin, Operand.Time tmax ) ->
                    if
                        Maybe.map2 inDateMapFn
                            (Time.parse tmin)
                            (Time.parse tmax)
                            == Just True
                    then
                        ( Nothing
                        , Just <| time name inDate (String.slice 0 10 tmin)
                        )

                    else
                        default

                _ ->
                    default
    in
    if name_ == name then
        case op of
            LesserOrEqual max ->
                case op_ of
                    GreaterOrEqual min ->
                        ( Nothing, Just <| Filter name (Between min max) )

                    _ ->
                        default

            GreaterOrEqual min ->
                case op_ of
                    LesserOrEqual max ->
                        ( Nothing, Just <| Filter name (Between min max) )

                    LesserThan max ->
                        makeInDate min max

                    _ ->
                        default

            LesserThan max ->
                case op_ of
                    GreaterOrEqual min ->
                        makeInDate min max

                    _ ->
                        default

            _ ->
                default

    else
        default



-- False


filterCons : Table -> (OperandConst -> Operation) -> String -> Maybe Filter
filterCons table operationCons name =
    case Dict.get name table |> Maybe.map columnValue of
        Just (PString _) ->
            Just <| Filter name <| operationCons Operand.text

        Just (PText _) ->
            Just <| Filter name <| operationCons Operand.text

        Just (PInt _) ->
            Just <| Filter name <| operationCons Operand.int

        Just (PFloat _) ->
            Just <| Filter name <| operationCons Operand.float

        Just (PBool _) ->
            Just <| Filter name <| operationCons Operand.text

        Just (PTime _) ->
            Just <| Filter name <| operationCons Operand.time

        Just (PDate _) ->
            Just <| Filter name <| operationCons Operand.date

        Just (PEnum _ choices) ->
            IsNull (Just <| NoneOf <| Operand.enum choices Set.empty)
                |> (Just << Filter name)

        _ ->
            Nothing


enumCons : Table -> (List String -> Operation) -> String -> Maybe Filter
enumCons table operationCons name =
    case Dict.get name table |> Maybe.map columnValue of
        Just (PEnum _ choices) ->
            if operationCons choices == IsNull Nothing then
                Just <| Filter name <| OneOf <| Operand.enum choices Set.empty

            else
                Just <| Filter name <| operationCons choices

        _ ->
            Nothing

module Filter exposing
    ( Filter
    , between
    , columnName
    , contains
    , date
    , endsWith
    , equals
    , float
    , fromColumn
    , greaterOrEqual
    , greaterThan
    , inDate
    , init
    , int
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
    , toPGQuery
    , toQueryString
    )

import Dict
import Filter.Operand as Operand exposing (Enum(..), Operand(..))
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
import Postgrest.Schema.Definition
    exposing
        ( Column(..)
        , Definition
        , columnValue
        )
import Postgrest.Value exposing (Value(..))
import Set exposing (Set)


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

        PPrimaryKey mprimaryKey ->
            Nothing

        PForeignKey mprimaryKey { label } ->
            Nothing

        BadValue _ ->
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



-- Operand constructors


float : String -> (Operand -> Operation) -> String -> Filter
float name operationCons value =
    init name (operationCons <| Operand.float value)


int : String -> (Operand -> Operation) -> String -> Filter
int name operationCons value =
    init name (operationCons <| Operand.int value)


text : String -> (Operand -> Operation) -> String -> Filter
text name operationCons value =
    init name (operationCons <| Operand.text value)


date : String -> (Operand -> Operation) -> String -> Filter
date name operationCons value =
    init name (operationCons <| Operand.date value)


time : String -> (Operand -> Operation) -> String -> Filter
time name operationCons value =
    init name (operationCons <| Operand.time value)


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


parse : Definition -> String -> Maybe Filter
parse definition query =
    query
        |> Parser.run (parseFilter definition)
        |> Result.mapError (Debug.log "error")
        |> Result.toMaybe
        |> Maybe.andThen identity


parseFilter : Definition -> Parser (Maybe Filter)
parseFilter definition =
    let
        colNames =
            Dict.keys definition
                |> List.map (\s -> succeed (always s) |= token s)
    in
    Parser.oneOf
        [ succeed (\name f -> f name)
            |= Parser.oneOf colNames
            |. symbol "="
            |= Parser.oneOf
                [ succeed (enumCons definition)
                    |= Filter.Parser.enum
                , succeed (filterCons definition)
                    |= Filter.Parser.operation
                ]
        ]



-- Parse helpers


filterCons : Definition -> (OperandConst -> Operation) -> String -> Maybe Filter
filterCons definition operationCons name =
    case Dict.get name definition |> Maybe.map columnValue of
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


enumCons : Definition -> (List String -> Operation) -> String -> Maybe Filter
enumCons definition operationCons name =
    case Dict.get name definition |> Maybe.map columnValue of
        Just (PEnum _ choices) ->
            if operationCons choices == IsNull Nothing then
                Just <| Filter name <| OneOf <| Operand.enum choices Set.empty

            else
                Just <| Filter name <| operationCons choices

        _ ->
            Nothing

module Filter exposing
    ( Filter(..)
    , between
    , contains
    , date
    , endsWith
    , equals
    , filter
    , float
    , fromColumn
    , greaterThan
    , inDate
    , int
    , isFalse
    , isNull
    , isTrue
    , lesserThan
    , noneOf
    , oneOf
    , parse
    , startsWith
    , text
    , time
    , toPGQuery
    )

import Dict
import Filter.Operand as Operand exposing (Enum, Operand(..))
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
        , variable
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


type alias OperationConst =
    Operand -> Operation


toPGQuery : Filter -> Maybe PG.Param
toPGQuery (Filter name op) =
    Operation.toPGQuery name op



-- Constructors


fromColumn : String -> Column -> Maybe Filter
fromColumn name column =
    case columnValue column of
        PString _ ->
            Just <| filter name equals <| text ""

        PText _ ->
            Just <| filter name equals <| text ""

        PInt _ ->
            Just <| filter name equals <| int ""

        PFloat _ ->
            Just <| filter name equals <| float ""

        PBool _ ->
            Just <| isTrue name

        PTime _ ->
            Just <| filter name inDate <| date ""

        PDate _ ->
            Just <| filter name inDate <| date ""

        PEnum _ choices ->
            Just <| oneOf name choices Set.empty

        PPrimaryKey mprimaryKey ->
            Nothing

        PForeignKey mprimaryKey { label } ->
            Nothing

        BadValue _ ->
            Nothing


filter : String -> (Operand -> Operation) -> Operand -> Filter
filter name operationCons operand =
    Filter name (operationCons operand)


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


date : String -> Operand
date =
    Operand.date


float : String -> Operand
float =
    Operand.float


int : String -> Operand
int =
    Operand.int


text : String -> Operand
text =
    Operand.text


time : String -> Operand
time =
    Operand.time


value : Operand -> String
value =
    Operand.value


isTrue : String -> Filter
isTrue name =
    Filter name IsTrue


isFalse : String -> Filter
isFalse name =
    Filter name IsFalse


isNull : String -> Filter
isNull name =
    Filter name (IsNull Nothing)



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
            Just <| Filter name <| IsNull <| Just <| NoneOf <| Operand.enum choices Set.empty

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

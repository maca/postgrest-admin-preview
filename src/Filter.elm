module Filter exposing
    ( Filter(..)
    , Kind(..)
    , between
    , bool
    , contains
    , date
    , endsWith
    , enum
    , equals
    , float
    , fromColumn
    , greaterThan
    , inDate
    , int
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


type Kind
    = IText
    | IInt
    | IFloat
    | IDate
    | ITime
    | IBool
    | IEnum


type Filter
    = Filter Kind String Operation


type alias OperandConst =
    String -> Operand


type alias OperationConst =
    Operand -> Operation


toPGQuery : Filter -> Maybe PG.Param
toPGQuery (Filter _ name op) =
    Operation.toPGQuery name op



-- Constructors


fromColumn : String -> Column -> Maybe Filter
fromColumn name column =
    case columnValue column of
        PString _ ->
            Just <| text name equals (Just "")

        PText _ ->
            Just <| text name equals (Just "")

        PInt _ ->
            Just <| int name equals (Just "")

        PFloat _ ->
            Just <| float name equals (Just "")

        PBool _ ->
            Just <| bool name (Just True)

        PTime _ ->
            Just <| time name inDate (Just "")

        PDate _ ->
            Just <| date name inDate (Just "")

        PEnum _ choices ->
            Just <| enum name oneOf choices Set.empty

        PPrimaryKey mprimaryKey ->
            Nothing

        PForeignKey mprimaryKey { label } ->
            Nothing

        BadValue _ ->
            Nothing


filter : String -> Kind -> OperationConst -> Maybe String -> Filter
filter name kind operationCons mstring =
    let
        fcons =
            Filter kind name
    in
    Maybe.map2 (\oc s -> fcons <| operationCons <| oc s)
        (operandConst kind)
        mstring
        |> Maybe.withDefault (fcons IsNull)


text : String -> (Operand -> Operation) -> Maybe String -> Filter
text name operationCons mstring =
    filter name IText operationCons mstring


int : String -> (Operand -> Operation) -> Maybe String -> Filter
int name operationCons mstring =
    filter name IInt operationCons mstring


float : String -> (Operand -> Operation) -> Maybe String -> Filter
float name operationCons mstring =
    filter name IFloat operationCons mstring


time : String -> (Operand -> Operation) -> Maybe String -> Filter
time name operationCons mstring =
    filter name ITime operationCons mstring


date : String -> (Operand -> Operation) -> Maybe String -> Filter
date name operationCons mstring =
    filter name IDate operationCons mstring


enum : String -> (Enum -> Operation) -> List String -> Set String -> Filter
enum name operationCons choices chosen =
    Filter IEnum name <| operationCons <| Operand.enum choices chosen


bool : String -> Maybe Bool -> Filter
bool name value =
    case value of
        Just True ->
            Filter IBool name IsTrue

        Just False ->
            Filter IBool name IsFalse

        Nothing ->
            Filter IBool name IsNull



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


oneOf : Enum -> Operation
oneOf =
    OneOf


noneOf : Enum -> Operation
noneOf =
    NoneOf



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
        , succeed Nothing
        ]


colName : Parser String
colName =
    variable
        { start = Char.isAlphaNum
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "and", "or", "order" ]
        }



-- Parse helpers


filterCons : Definition -> (OperandConst -> Operation) -> String -> Maybe Filter
filterCons definition operationCons name =
    case Dict.get name definition |> Maybe.map columnValue of
        Just (PString _) ->
            Just <| Filter IText name <| operationCons Operand.text

        Just (PText _) ->
            Just <| Filter IText name <| operationCons Operand.text

        Just (PInt _) ->
            Just <| Filter IInt name <| operationCons Operand.int

        Just (PFloat _) ->
            Just <| Filter IFloat name <| operationCons Operand.float

        Just (PBool _) ->
            Just <| Filter IBool name <| operationCons Operand.text

        Just (PTime _) ->
            Just <| Filter ITime name <| operationCons Operand.time

        Just (PDate _) ->
            Just <| Filter IDate name <| operationCons Operand.date

        _ ->
            Nothing


enumCons : Definition -> (List String -> Operation) -> String -> Maybe Filter
enumCons definition operationCons name =
    Dict.get name definition
        |> Maybe.andThen (enumConsHelp operationCons name)


enumConsHelp : (List String -> Operation) -> String -> Column -> Maybe Filter
enumConsHelp operationCons name column =
    case columnValue column of
        PEnum _ choices ->
            Just <| Filter IEnum name <| operationCons choices

        _ ->
            Nothing



-- Helpers


operandConst : Kind -> Maybe (String -> Operand)
operandConst kind =
    case kind of
        IText ->
            Just Operand.text

        IInt ->
            Just Operand.int

        IFloat ->
            Just Operand.float

        IDate ->
            Just Operand.date

        ITime ->
            Just Operand.time

        IBool ->
            Nothing

        IEnum ->
            Nothing

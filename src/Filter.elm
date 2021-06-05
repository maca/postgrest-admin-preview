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
    , nullBool
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
import Filter.Parser exposing (OperandConst)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Trailing(..)
        , succeed
        , symbol
        , variable
        )
import Postgrest.Client as PG
import Postgrest.Schema.Definition exposing (Column(..), Definition)
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


toPGQuery : Filter -> Maybe PG.Param
toPGQuery (Filter _ name op) =
    Operation.toPGQuery name op



-- Constructors


fromColumn : String -> Column -> Maybe Filter
fromColumn name (Column _ value) =
    case value of
        PString _ ->
            Just <| text name equals Nothing

        PText _ ->
            Just <| text name equals Nothing

        PInt _ ->
            Just <| int name equals Nothing

        PFloat _ ->
            Just <| float name <| equals <| Operand.float Nothing

        PBool _ ->
            Just <| bool name True

        PTime _ ->
            Just <| time name <| inDate <| Operand.time Nothing

        PDate _ ->
            Just <| date name <| inDate <| Operand.date Nothing

        PEnum _ choices ->
            Just <| enum name oneOf choices Set.empty

        PPrimaryKey mprimaryKey ->
            Nothing

        PForeignKey mprimaryKey { label } ->
            Nothing

        BadValue _ ->
            Nothing


text : String -> (Operand -> Operation) -> Maybe String -> Filter
text name makeOperation mstring =
    Filter IText name <| makeOperation <| Operand.text mstring


int : String -> (Operand -> Operation) -> Maybe String -> Filter
int name makeOperation mstring =
    Filter IInt name <| makeOperation <| Operand.int mstring


float : String -> Operation -> Filter
float name =
    Filter IFloat name


bool : String -> Bool -> Filter
bool name value =
    if value then
        Filter IBool name IsTrue

    else
        Filter IBool name IsFalse


nullBool : String -> Filter
nullBool name =
    Filter IBool name IsNull


time : String -> Operation -> Filter
time name =
    Filter ITime name


date : String -> Operation -> Filter
date name =
    Filter IDate name


enum : String -> (Enum -> Operation) -> List String -> Set String -> Filter
enum name makeOperation choices chosen =
    Filter IEnum name <| makeOperation <| Operand.enum choices chosen


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
parse definition fragment =
    fragment
        |> Parser.run
            (Parser.oneOf
                [ succeed (\name f -> f name)
                    |= colName
                    |. symbol "="
                    |= Parser.oneOf
                        [ succeed (enumCons definition)
                            |= Filter.Parser.enum
                        , succeed (filterCons definition)
                            |= Filter.Parser.operation
                        ]
                ]
            )
        |> Result.mapError (Debug.log "error")
        |> Result.toMaybe
        |> Maybe.andThen identity


colName : Parser String
colName =
    variable
        { start = Char.isAlphaNum
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.fromList [ "and", "or", "order" ]
        }



-- Parse helpers


filterCons : Definition -> (OperandConst -> Operation) -> String -> Maybe Filter
filterCons definition makeOperation name =
    Dict.get name definition
        |> Maybe.andThen (filterConsHelp makeOperation name)


filterConsHelp : (OperandConst -> Operation) -> String -> Column -> Maybe Filter
filterConsHelp makeOperation name (Column _ value) =
    case value of
        PString _ ->
            Just <| Filter IText name <| makeOperation Operand.text

        PText _ ->
            Just <| Filter IText name <| makeOperation Operand.text

        PInt _ ->
            Just <| Filter IInt name <| makeOperation Operand.int

        PFloat _ ->
            Just <| Filter IFloat name <| makeOperation Operand.float

        PBool _ ->
            Just <| Filter IBool name <| makeOperation Operand.text

        PTime _ ->
            Just <| Filter ITime name <| makeOperation Operand.time

        PDate _ ->
            Just <| Filter IDate name <| makeOperation Operand.date

        _ ->
            Nothing


enumCons : Definition -> (List String -> Operation) -> String -> Maybe Filter
enumCons definition makeOperation name =
    Dict.get name definition
        |> Maybe.andThen (enumConsHelp makeOperation name)


enumConsHelp : (List String -> Operation) -> String -> Column -> Maybe Filter
enumConsHelp makeOperation name (Column _ value) =
    case value of
        PEnum _ choices ->
            Just <| Filter IEnum name <| makeOperation choices

        _ ->
            Nothing

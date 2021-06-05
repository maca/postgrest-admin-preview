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
    , startsWith
    , text
    , time
    , toPGQuery
    )

import Filter.Operand as Operand exposing (Enum, Operand)
import Filter.Operation as Operation exposing (Operation(..))
import Postgrest.Client as PG
import Postgrest.Schema.Definition exposing (Column(..))
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


fromColumn : String -> Column -> Maybe Filter
fromColumn name (Column _ value) =
    case value of
        PString _ ->
            Just <| text name Equals Nothing

        PText _ ->
            Just <| text name Equals Nothing

        PInt _ ->
            Just <| int name Equals Nothing

        PFloat _ ->
            Just <| float name <| Equals <| Operand.float Nothing

        PBool _ ->
            Just <| bool name True

        PTime _ ->
            Just <| time name <| InDate <| Operand.time Nothing

        PDate _ ->
            Just <| date name <| InDate <| Operand.date Nothing

        PEnum _ choices ->
            Just <| enum name OneOf choices Set.empty

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

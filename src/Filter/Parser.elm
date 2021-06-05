module Filter.Parser exposing (parse)

import Dict
import Filter exposing (Filter(..), Kind(..))
import Filter.Operand as Operand exposing (Enum, Operand(..))
import Filter.Operation exposing (Operation(..))
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Trailing(..)
        , backtrackable
        , chompUntil
        , chompUntilEndOr
        , getChompedString
        , spaces
        , succeed
        , symbol
        , token
        , variable
        )
import Postgrest.Schema.Definition exposing (Column(..), Definition)
import Postgrest.Value exposing (Value(..))
import Set exposing (Set)
import String.Extra as String
import Url exposing (percentDecode)


type alias OperandConst =
    Maybe String -> Operand


parse : Definition -> String -> Maybe Filter
parse definition fragment =
    fragment
        |> Parser.run
            (Parser.oneOf
                [ succeed (\name f -> f name)
                    |= colName
                    |. symbol "="
                    |= Parser.oneOf
                        [ succeed (enumCons definition) |= enum
                        , succeed (filterCons definition) |= operation
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


operation : Parser (OperandConst -> Operation)
operation =
    Parser.oneOf
        [ contains
        , endsWith
        , startsWith
        , is
        , equals
        , lesserThan
        , greaterThan
        ]


contains : Parser (OperandConst -> Operation)
contains =
    succeed (operationCons Contains)
        |. token "ilike"
        |. symbol ".*"
        |= getUntil "*"
        |> backtrackable


endsWith : Parser (OperandConst -> Operation)
endsWith =
    succeed (operationCons EndsWith)
        |. token "ilike"
        |. symbol ".*"
        |= string
        |> backtrackable


startsWith : Parser (OperandConst -> Operation)
startsWith =
    succeed (operationCons StartsWith)
        |. token "ilike"
        |. symbol "."
        |= getUntil "*"


is : Parser (OperandConst -> Operation)
is =
    succeed identity
        |. token "is"
        |. symbol "."
        |= Parser.oneOf [ true, false, null ]


true : Parser (OperandConst -> Operation)
true =
    succeed (always IsTrue) |. token "true"


false : Parser (OperandConst -> Operation)
false =
    succeed (always IsFalse) |. token "false"


null : Parser (OperandConst -> Operation)
null =
    succeed (always IsNull) |. token "null"


equals : Parser (OperandConst -> Operation)
equals =
    succeed (unquoteMaybe >> operationCons Equals)
        |. token "eq"
        |. symbol "."
        |= string


lesserThan : Parser (OperandConst -> Operation)
lesserThan =
    succeed (unquoteMaybe >> operationCons LesserThan)
        |. token "lt"
        |. symbol "."
        |= string


greaterThan : Parser (OperandConst -> Operation)
greaterThan =
    succeed (unquoteMaybe >> operationCons GreaterThan)
        |. token "gt"
        |. symbol "."
        |= string


enum : Parser (List String -> Operation)
enum =
    Parser.oneOf
        [ succeed (Set.fromList >> enumOperationCons OneOf)
            |. token "in"
            |. symbol "."
            |= items
        , succeed (Set.fromList >> enumOperationCons NoneOf)
            |. token "not.in"
            |. symbol "."
            |= items
        ]


items : Parser (List String)
items =
    Parser.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = spaces
        , item = quoted
        , trailing = Forbidden
        }


quoted : Parser String
quoted =
    succeed (Maybe.withDefault "") |= getUntil "\""



-- |= variable
--     { start = always True
--     , inner = (/=) ','
--     , reserved = Set.fromList []
--     }
-- |. token "\""


unquoteMaybe : Maybe String -> Maybe String
unquoteMaybe =
    Maybe.map String.unquote


string : Parser (Maybe String)
string =
    succeed percentDecode
        |= (getChompedString <| succeed () |. chompUntilEndOr "\n")


getUntil : String -> Parser (Maybe String)
getUntil terminator =
    succeed percentDecode
        |= (getChompedString <| succeed () |. chompUntil terminator)


operationCons :
    (Operand -> Operation)
    -> Maybe String
    -> OperandConst
    -> Operation
operationCons makeOperation mstring makeOperator =
    makeOperation <| makeOperator mstring


enumOperationCons : (Enum -> Operation) -> Set String -> List String -> Operation
enumOperationCons makeOperation chosen choices =
    makeOperation <| Operand.enum choices chosen


filterCons : Definition -> (OperandConst -> Operation) -> String -> Maybe Filter
filterCons definition makeOperation name =
    case Dict.get name definition of
        Just (Column _ value) ->
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

        Nothing ->
            Nothing


enumCons : Definition -> (List String -> Operation) -> String -> Maybe Filter
enumCons definition makeOperation name =
    case Dict.get name definition of
        Just (Column _ value) ->
            case value of
                PEnum _ choices ->
                    Just <| Filter IEnum name <| makeOperation choices

                _ ->
                    Nothing

        Nothing ->
            Nothing

module Filter.Parser exposing (OperandConst, enum, operation)

import Filter.Operand as Operand exposing (Enum, Operand(..))
import Filter.Operation exposing (Operation(..))
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Trailing(..)
        , backtrackable
        , chompIf
        , chompUntil
        , chompUntilEndOr
        , getChompedString
        , spaces
        , succeed
        , symbol
        , token
        , variable
        )
import Postgrest.Schema.Definition exposing (Column(..))
import Postgrest.Value exposing (Value(..))
import Set exposing (Set)
import String.Extra as String


type alias OperandConst =
    Maybe String -> Operand


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
        |= getRest
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
        |= getRest


lesserThan : Parser (OperandConst -> Operation)
lesserThan =
    succeed (unquoteMaybe >> operationCons LesserThan)
        |. token "lt"
        |. symbol "."
        |= getRest


greaterThan : Parser (OperandConst -> Operation)
greaterThan =
    succeed (unquoteMaybe >> operationCons GreaterThan)
        |. token "gt"
        |. symbol "."
        |= getRest


enum : Parser (List String -> Operation)
enum =
    Parser.oneOf
        [ succeed (Set.fromList >> enumOperationCons OneOf)
            |. token "in."
            |= items
        , succeed (Set.fromList >> enumOperationCons NoneOf)
            |. token "not.in."
            |= items
        ]


items : Parser (List String)
items =
    Parser.succeed identity
        |= Parser.sequence
            { start = "("
            , separator = ","
            , end = ")"
            , spaces = Parser.spaces
            , item = item
            , trailing = Forbidden
            }


item : Parser String
item =
    Parser.oneOf
        [ succeed identity |= string '"'
        , succeed identity
            |= variable
                { start = always True
                , inner = (/=) ','
                , reserved = Set.fromList []
                }
        ]


string : Char -> Parser String
string delimiter =
    Parser.succeed ()
        |. token (String.fromChar delimiter)
        |. Parser.loop delimiter stringHelp
        |> getChompedString
        -- Remove quotes
        |> Parser.map (String.dropLeft 1 >> String.dropRight 1)


stringHelp : Char -> Parser (Parser.Step Char ())
stringHelp delimiter =
    Parser.oneOf
        [ succeed (Parser.Done ())
            |. token (String.fromChar delimiter)
        , succeed (Parser.Loop delimiter)
            |. chompIf ((==) '\\')
            |. chompIf (always True)
        , succeed (Parser.Loop delimiter)
            |. chompIf (\char -> char /= '\\' && char /= delimiter)
        ]


unquoteMaybe : Maybe String -> Maybe String
unquoteMaybe =
    Maybe.map String.unquote


getRest : Parser (Maybe String)
getRest =
    succeed Just
        |= (getChompedString <| succeed () |. chompUntilEndOr "\n")


getUntil : String -> Parser (Maybe String)
getUntil terminator =
    succeed Just
        |= (getChompedString <| succeed () |. chompUntil terminator)


operationCons :
    (Operand -> Operation)
    -> Maybe String
    -> OperandConst
    -> Operation
operationCons makeOperation mstring makeOperator =
    makeOperation <| makeOperator mstring


enumOperationCons :
    (Enum -> Operation)
    -> Set String
    -> List String
    -> Operation
enumOperationCons makeOperation chosen choices =
    makeOperation <| Operand.enum choices chosen

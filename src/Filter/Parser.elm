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
import String.Extra exposing (unquote)
import Url exposing (percentDecode)


type alias OperandConst =
    String -> Operand


operation : Parser (OperandConst -> Operation)
operation =
    Parser.oneOf
        [ contains
        , endsWith
        , startsWith
        , is
        , equals
        , inTheFuture
        , inThePast
        , lesserThan
        , greaterThan
        , lesserOrEqual
        , greaterOrEqual
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
    succeed (always (IsNull Nothing)) |. token "null"


equals : Parser (OperandConst -> Operation)
equals =
    succeed (unquote >> operationCons Equals)
        |. token "eq"
        |. symbol "."
        |= getRest


inTheFuture : Parser (OperandConst -> Operation)
inTheFuture =
    succeed (always <| IsInTheFuture Nothing)
        |. token "gt.now"


inThePast : Parser (OperandConst -> Operation)
inThePast =
    succeed (always <| IsInThePast Nothing)
        |. token "lt.now"


lesserThan : Parser (OperandConst -> Operation)
lesserThan =
    succeed (unquote >> operationCons LesserThan)
        |. symbol "lt."
        |= getRest


greaterThan : Parser (OperandConst -> Operation)
greaterThan =
    succeed (unquote >> operationCons GreaterThan)
        |. symbol "gt."
        |= getRest


lesserOrEqual : Parser (OperandConst -> Operation)
lesserOrEqual =
    succeed (unquote >> operationCons LesserOrEqual)
        |. symbol "lte."
        |= getRest


greaterOrEqual : Parser (OperandConst -> Operation)
greaterOrEqual =
    succeed (unquote >> operationCons GreaterOrEqual)
        |. symbol "gte."
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
    Parser.sequence
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


getRest : Parser String
getRest =
    succeed (percentDecode >> Maybe.withDefault "")
        |= (getChompedString <| succeed () |. chompUntilEndOr "\n")


getUntil : String -> Parser String
getUntil terminator =
    succeed (percentDecode >> Maybe.withDefault "")
        |= (getChompedString <| succeed () |. chompUntil terminator)


operationCons : (Operand -> Operation) -> String -> OperandConst -> Operation
operationCons makeOperation val makeOperator =
    makeOperation <| makeOperator val


enumOperationCons :
    (Enum -> Operation)
    -> Set String
    -> List String
    -> Operation
enumOperationCons makeOperation chosen choices =
    makeOperation <| Operand.enum choices chosen

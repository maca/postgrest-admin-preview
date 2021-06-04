module Filter exposing
    ( Filter(..)
    , Kind(..)
    , fromColumn
    , parse
    , toPGQuery
    )

import Dict
import Filter.Operand as Operand exposing (Operand(..))
import Filter.Operation as Operation exposing (Operation(..))
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , backtrackable
        , chompUntil
        , chompUntilEndOr
        , getChompedString
        , succeed
        , symbol
        , token
        , variable
        )
import Postgrest.Client as PG
import Postgrest.Schema.Definition exposing (Column(..), Definition)
import Postgrest.Value exposing (Value(..))
import Set
import String.Extra as String
import Url exposing (percentDecode)


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
    Maybe String -> Operand


toPGQuery : Filter -> Maybe PG.Param
toPGQuery (Filter _ name op) =
    Operation.toPGQuery name op


fromColumn : String -> Column -> Maybe Filter
fromColumn name (Column _ value) =
    case value of
        PString _ ->
            Just <| Filter IText name <| Equals <| Operand.text Nothing

        PText _ ->
            Just <| Filter IText name <| Equals <| Operand.text Nothing

        PInt _ ->
            Just <| Filter IInt name <| Equals <| Operand.int Nothing

        PFloat _ ->
            Just <| Filter IFloat name <| Equals <| Operand.float Nothing

        PBool _ ->
            Just <| Filter IBool name <| IsTrue

        PEnum _ choices ->
            Just <| Filter IEnum name <| OneOf <| Operand.enum choices Set.empty

        PTime _ ->
            Just <| Filter ITime name <| InDate <| Operand.time Nothing

        PDate _ ->
            Just <| Filter IDate name <| InDate <| Operand.date Nothing

        PPrimaryKey mprimaryKey ->
            Nothing

        PForeignKey mprimaryKey { label } ->
            Nothing

        BadValue _ ->
            Nothing


parse : Definition -> String -> Maybe Filter
parse definition fragment =
    fragment
        |> Parser.run
            (succeed identity
                |= Parser.oneOf
                    [ succeed (filterCons definition)
                        |= colName
                        |. symbol "="
                        |= operation
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


filterCons : Definition -> String -> (OperandConst -> Operation) -> Maybe Filter
filterCons definition name makeOperation =
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

                PEnum _ choices ->
                    Nothing

                -- Just <| Filter IEnum name <| OneOf <| Operand.enum choices Set.empty
                PPrimaryKey mprimaryKey ->
                    Nothing

                PForeignKey mprimaryKey { label } ->
                    Nothing

                BadValue _ ->
                    Nothing

        Nothing ->
            Nothing

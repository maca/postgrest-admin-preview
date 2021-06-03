module Filter exposing
    ( Filter(..)
    , fromColumn
    , parse
    , reassign
    , toPGQuery
    , toString
    )

import Dict
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


type Filter
    = TextFilter String Operation
    | IntFilter String Operation
    | FloatFilter String Operation
    | BoolFilter String Operation
    | DateFilter String Operation
    | TimeFilter String Operation
    | EnumFilter String (List String) Operation


toString : Filter -> String
toString filter =
    case filter of
        TextFilter _ _ ->
            "text"

        IntFilter _ _ ->
            "integer"

        FloatFilter _ _ ->
            "float"

        BoolFilter _ _ ->
            "bool"

        DateFilter _ _ ->
            "date"

        TimeFilter _ _ ->
            "time"

        EnumFilter _ _ _ ->
            "enum"


toPGQuery : Filter -> Maybe PG.Param
toPGQuery filter =
    case filter of
        TextFilter name op ->
            Operation.toPGQuery name op

        IntFilter name op ->
            Operation.toPGQuery name op

        FloatFilter name op ->
            Operation.toPGQuery name op

        BoolFilter name op ->
            Operation.toPGQuery name op

        DateFilter name op ->
            Operation.toPGQuery name op

        TimeFilter name op ->
            Operation.toPGQuery name op

        EnumFilter name _ op ->
            Operation.toPGQuery name op


reassign : String -> Filter -> Filter
reassign name filter =
    case filter of
        TextFilter _ op ->
            TextFilter name op

        IntFilter _ op ->
            IntFilter name op

        FloatFilter _ op ->
            FloatFilter name op

        BoolFilter _ op ->
            BoolFilter name op

        DateFilter _ op ->
            TimeFilter name op

        TimeFilter _ op ->
            TimeFilter name op

        EnumFilter _ choices op ->
            EnumFilter name choices op


fromColumn : String -> Column -> Maybe Filter
fromColumn name ((Column _ value) as column) =
    let
        defaultOp =
            case value of
                PBool _ ->
                    IsTrue

                PEnum _ choices ->
                    OneOf Set.empty

                PTime _ ->
                    InDate Nothing

                PDate _ ->
                    InDate Nothing

                _ ->
                    Equals Nothing
    in
    init name column defaultOp


init : String -> Column -> Operation -> Maybe Filter
init name (Column _ value) =
    case value of
        PString _ ->
            Just << TextFilter name

        PText _ ->
            Just << TextFilter name

        PInt _ ->
            Just << IntFilter name

        PFloat _ ->
            Just << FloatFilter name

        PBool _ ->
            Just << BoolFilter name

        PEnum _ choices ->
            Just << EnumFilter name choices

        PTime _ ->
            Just << TimeFilter name

        PDate _ ->
            Just << TimeFilter name

        PPrimaryKey mprimaryKey ->
            always Nothing

        PForeignKey mprimaryKey { label } ->
            always Nothing

        BadValue _ ->
            always Nothing


parse : Definition -> String -> Maybe Filter
parse definition fragment =
    let
        makeFilter name =
            case Dict.get name definition of
                Just column ->
                    init name column

                Nothing ->
                    always Nothing
    in
    fragment
        |> Parser.run
            (succeed identity
                |= Parser.oneOf
                    [ succeed makeFilter |= colName |. symbol "=" |= operation ]
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


operation : Parser Operation
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


contains : Parser Operation
contains =
    succeed Contains
        |. token "ilike"
        |. symbol ".*"
        |= getUntil "*"
        |> backtrackable


endsWith : Parser Operation
endsWith =
    succeed EndsWith
        |. token "ilike"
        |. symbol ".*"
        |= string
        |> backtrackable


startsWith : Parser Operation
startsWith =
    succeed StartsWith |. token "ilike" |. symbol "." |= getUntil "*"


is : Parser Operation
is =
    succeed identity
        |. token "is"
        |. symbol "."
        |= Parser.oneOf [ true, false, null ]


true : Parser Operation
true =
    succeed IsTrue |. token "true"


false : Parser Operation
false =
    succeed IsFalse |. token "false"


null : Parser Operation
null =
    succeed IsNull |. token "null"


equals : Parser Operation
equals =
    succeed (unquoteMaybe >> Equals) |. token "eq" |. symbol "." |= string


lesserThan : Parser Operation
lesserThan =
    succeed (unquoteMaybe >> LesserThan) |. token "lt" |. symbol "." |= string


greaterThan : Parser Operation
greaterThan =
    succeed (unquoteMaybe >> GreaterThan) |. token "gt" |. symbol "." |= string


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

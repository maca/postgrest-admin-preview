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
        )
import Postgrest.Client as PG
import Postgrest.Schema.Definition exposing (Column(..), Definition)
import Postgrest.Value exposing (Value(..))
import Set
import Url exposing (percentDecode)


type Filter
    = TextFilter String Operation
    | IntFilter String Operation
    | FloatFilter String Operation
    | BoolFilter String Operation
    | DateFilter String Operation
    | TimeFilter String Operation
    | EnumFilter String (List String) Operation
    | Blank


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

        Blank ->
            ""


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

        Blank ->
            Nothing


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

        Blank ->
            Blank


fromColumn : String -> Column -> Filter
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


init : String -> Column -> Operation -> Filter
init name (Column _ value) =
    case value of
        PString _ ->
            TextFilter name

        PText _ ->
            TextFilter name

        PInt _ ->
            IntFilter name

        PFloat _ ->
            FloatFilter name

        PBool _ ->
            BoolFilter name

        PEnum _ choices ->
            EnumFilter name choices

        PTime _ ->
            TimeFilter name

        PDate _ ->
            TimeFilter name

        PPrimaryKey mprimaryKey ->
            always Blank

        PForeignKey mprimaryKey { label } ->
            always Blank

        BadValue _ ->
            always Blank


parse : Definition -> ( String, String ) -> Filter
parse definition ( name, fragment ) =
    let
        makeFilter =
            Dict.get name definition
                |> Maybe.map (init name)
                |> Maybe.withDefault (always Blank)
    in
    fragment
        |> Parser.run
            (succeed makeFilter
                |= Parser.oneOf
                    [ contains
                    , startsWith
                    , endsWith
                    , is
                    , equals
                    , lesserThan
                    , greaterThan
                    ]
            )
        |> Result.mapError (Debug.log "error")
        |> Result.withDefault Blank


contains : Parser Operation
contains =
    succeed Contains
        |. token "ilike"
        |. symbol ".*"
        |= getUntil "*"
        |> backtrackable


startsWith : Parser Operation
startsWith =
    succeed StartsWith
        |. token "ilike"
        |. symbol ".*"
        |= string
        |> backtrackable


endsWith : Parser Operation
endsWith =
    succeed EndsWith |. token "ilike" |. symbol "." |= getUntil "*"


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
    succeed Equals |. token "eq" |. symbol "." |= string


lesserThan : Parser Operation
lesserThan =
    succeed LesserThan |. token "lt" |. symbol "." |= string


greaterThan : Parser Operation
greaterThan =
    succeed GreaterThan |. token "gt" |. symbol "." |= string


string : Parser (Maybe String)
string =
    succeed percentDecode
        |= (getChompedString <| succeed () |. chompUntilEndOr "\n")


getUntil : String -> Parser (Maybe String)
getUntil terminator =
    succeed percentDecode
        |= (getChompedString <| succeed () |. chompUntil terminator)

module Filter exposing
    ( Filter(..)
    , Kind(..)
    , fromColumn
    , toPGQuery
    )

import Filter.Operand as Operand exposing (Operand(..))
import Filter.Operation as Operation exposing (Operation(..))
import Postgrest.Client as PG
import Postgrest.Schema.Definition exposing (Column(..))
import Postgrest.Value exposing (Value(..))
import Set


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

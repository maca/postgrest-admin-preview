module Filter exposing (Filter(..), fromColumn, reassign, toString)

import Filter.Operator exposing (Operator(..))
import Postgrest.Schema.Definition exposing (Column(..))
import Postgrest.Value exposing (Value(..))
import Set


type Filter
    = TextFilter String Operator
    | IntFilter String Operator
    | FloatFilter String Operator
    | BoolFilter String Operator
    | DateFilter String Operator
    | TimeFilter String Operator
    | EnumFilter String Operator
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

        EnumFilter _ _ ->
            "enum"

        Blank ->
            ""


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

        EnumFilter _ op ->
            EnumFilter name op

        Blank ->
            Blank


fromColumn : String -> Column -> Filter
fromColumn name (Column _ value) =
    case value of
        PString _ ->
            TextFilter name <| Equals Nothing

        PText _ ->
            TextFilter name <| Equals Nothing

        PInt _ ->
            IntFilter name <| Equals Nothing

        PFloat _ ->
            FloatFilter name <| Equals Nothing

        PBool _ ->
            BoolFilter name IsTrue

        PEnum _ choices ->
            EnumFilter name <| OneOf choices Set.empty

        PTime _ ->
            TimeFilter name <| InDate Nothing

        PDate _ ->
            TimeFilter name <| InDate Nothing

        PPrimaryKey mprimaryKey ->
            Blank

        PForeignKey mprimaryKey { label } ->
            Blank

        BadValue _ ->
            Blank

module Listing.Search.Filter exposing (Filter(..), fromColumn, reassign, toString)

import Listing.Search.Bool exposing (BoolOp(..))
import Listing.Search.Enum exposing (EnumOp(..))
import Listing.Search.Num exposing (NumOp(..))
import Listing.Search.Text exposing (TextOp(..))
import Listing.Search.Time exposing (TimeOp(..))
import Postgrest.Schema.Definition exposing (Column(..), Definition)
import Postgrest.Value as Value exposing (Value(..))


type Filter
    = TextFilter String TextOp
    | NumFilter String NumOp
    | BoolFilter String BoolOp
    | EnumFilter String EnumOp
    | TimeFilter String TimeOp
    | Blank


toString : Filter -> String
toString filter =
    case filter of
        TextFilter _ _ ->
            "text"

        NumFilter _ _ ->
            "number"

        BoolFilter _ _ ->
            "bool"

        EnumFilter _ _ ->
            "enum"

        TimeFilter _ _ ->
            "time"

        Blank ->
            ""


reassign : String -> Filter -> Filter
reassign name filter =
    case filter of
        TextFilter _ op ->
            TextFilter name op

        NumFilter _ op ->
            NumFilter name op

        BoolFilter _ op ->
            BoolFilter name op

        EnumFilter _ op ->
            EnumFilter name op

        TimeFilter _ op ->
            TimeFilter name op

        Blank ->
            Blank


fromColumn : String -> Column -> Filter
fromColumn name (Column _ value) =
    case value of
        PString _ ->
            TextFilter name <| TextEquals Nothing

        PText _ ->
            TextFilter name <| TextEquals Nothing

        PFloat _ ->
            NumFilter name <| NumEquals Nothing

        PInt _ ->
            NumFilter name <| NumEquals Nothing

        PBool _ ->
            Blank

        PEnum _ _ ->
            EnumFilter name EnumAll

        PTime _ ->
            Blank

        PDate _ ->
            Blank

        PPrimaryKey mprimaryKey ->
            Blank

        PForeignKey mprimaryKey { label } ->
            Blank

        BadValue _ ->
            Blank

module Listing.Search.Filter exposing
    ( Filter(..)
    , fromColumn
    , reassign
    , toString
    )

import Listing.Search.Bool exposing (BoolOp(..))
import Listing.Search.Enum exposing (EnumOp(..))
import Listing.Search.Num exposing (NumInput(..), NumOp(..))
import Listing.Search.Text exposing (TextOp(..))
import Listing.Search.Time exposing (TimeInput(..), TimeOp(..))
import Postgrest.Schema.Definition exposing (Column(..), Definition)
import Postgrest.Value as Value exposing (Value(..))


type Filter
    = TextFilter String TextOp
    | NumFilter String NumInput NumOp
    | BoolFilter String BoolOp
    | EnumFilter String EnumOp
    | TimeFilter String TimeInput TimeOp
    | Blank


toString : Filter -> String
toString filter =
    case filter of
        TextFilter _ _ ->
            "text"

        NumFilter _ _ _ ->
            "number"

        BoolFilter _ _ ->
            "bool"

        EnumFilter _ _ ->
            "enum"

        TimeFilter _ _ _ ->
            "time"

        Blank ->
            ""


reassign : String -> Filter -> Filter
reassign name filter =
    case filter of
        TextFilter _ op ->
            TextFilter name op

        NumFilter _ inputType op ->
            NumFilter name inputType op

        BoolFilter _ op ->
            BoolFilter name op

        EnumFilter _ op ->
            EnumFilter name op

        TimeFilter _ inputType op ->
            TimeFilter name inputType op

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
            NumFilter name FloatInput <| NumEquals Nothing

        PInt _ ->
            NumFilter name IntInput <| NumEquals Nothing

        PBool _ ->
            Blank

        PEnum _ _ ->
            EnumFilter name EnumAll

        PTime _ ->
            TimeFilter name TimeInput <| TimeInDate Nothing

        PDate _ ->
            TimeFilter name DateInput <| TimeInDate Nothing

        PPrimaryKey mprimaryKey ->
            Blank

        PForeignKey mprimaryKey { label } ->
            Blank

        BadValue _ ->
            Blank

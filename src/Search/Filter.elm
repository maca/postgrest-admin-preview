module Search.Filter exposing (Filter(..), fromColumn, reassign, toString)

import Postgrest.Schema.Definition exposing (Column(..))
import Postgrest.Value exposing (Value(..))
import Search.Bool as FBool exposing (BoolOp)
import Search.Enum as FEnum exposing (EnumOp)
import Search.Num as FNum exposing (NumInput(..), NumOp)
import Search.Text as FText exposing (TextOp)
import Search.Time as FTime exposing (TimeInput(..), TimeOp)


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
            TextFilter name FText.init

        PText _ ->
            TextFilter name FText.init

        PFloat _ ->
            NumFilter name FloatInput FNum.init

        PInt _ ->
            NumFilter name IntInput FNum.init

        PBool _ ->
            BoolFilter name FBool.init

        PEnum _ choices ->
            EnumFilter name <| FEnum.init choices

        PTime _ ->
            TimeFilter name TimeInput FTime.init

        PDate _ ->
            TimeFilter name DateInput FTime.init

        PPrimaryKey mprimaryKey ->
            Blank

        PForeignKey mprimaryKey { label } ->
            Blank

        BadValue _ ->
            Blank

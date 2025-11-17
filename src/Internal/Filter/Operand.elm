module Internal.Filter.Operand exposing
    ( Enum
    , Operand(..)
    , choices
    , chosen
    , constructor
    , date
    , enum
    , float
    , int
    , rawValue
    , text
    , time
    , value
    )

import Set exposing (Set)


type Operand
    = Text String
    | Int String
    | Float String
    | Date String
    | Time String


type Enum
    = Enum (List String) (Set String)



-- Constructors


text : String -> Operand
text =
    Text


int : String -> Operand
int =
    Int


float : String -> Operand
float =
    Float


date : String -> Operand
date =
    Date


time : String -> Operand
time =
    Time


enum : List String -> Set String -> Enum
enum =
    Enum



-- Access


choices : Enum -> List String
choices (Enum list _) =
    list


chosen : Enum -> Set String
chosen (Enum _ set) =
    set


value : Operand -> String
value operand =
    case operand of
        Text val ->
            val

        Int val ->
            val

        Float val ->
            val

        Date val ->
            val

        Time val ->
            val


rawValue : Operand -> String
rawValue operand =
    case operand of
        Text val ->
            val

        Int val ->
            val

        Float val ->
            val

        Date val ->
            val

        Time val ->
            val


constructor : Operand -> (String -> Operand)
constructor operand =
    case operand of
        Text _ ->
            text

        Int _ ->
            int

        Float _ ->
            float

        Date _ ->
            date

        Time _ ->
            time


isText : Operand -> Bool
isText operand =
    case operand of
        Text _ ->
            True

        _ ->
            False


isInt : Operand -> Bool
isInt operand =
    case operand of
        Int _ ->
            True

        _ ->
            False


isFloat : Operand -> Bool
isFloat operand =
    case operand of
        Float _ ->
            True

        _ ->
            False


isDate : Operand -> Bool
isDate operand =
    case operand of
        Date _ ->
            True

        _ ->
            False


isTime : Operand -> Bool
isTime operand =
    case operand of
        Time _ ->
            True

        _ ->
            False

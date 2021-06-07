module Filter.Operand exposing
    ( Enum
    , Operand
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

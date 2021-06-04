module Filter.Operand exposing
    ( Enum(..)
    , Operand(..)
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
    = OText (Maybe String)
    | OInt (Maybe String)
    | OFloat (Maybe String)
    | ODate (Maybe String)
    | OTime (Maybe String)
    | NullOperand


type Enum
    = Enum (List String) (Set String)


text : Maybe String -> Operand
text =
    OText


int : Maybe String -> Operand
int =
    OInt


float : Maybe String -> Operand
float =
    OFloat


date : Maybe String -> Operand
date =
    ODate


time : Maybe String -> Operand
time =
    OTime


enum : List String -> Set String -> Enum
enum =
    Enum


value : Operand -> Maybe String
value operand =
    case operand of
        OText val ->
            val

        OInt val ->
            val

        OFloat val ->
            val

        ODate val ->
            val

        OTime val ->
            val

        NullOperand ->
            Nothing


rawValue : Operand -> Maybe String
rawValue operand =
    case operand of
        OText val ->
            val

        OInt val ->
            val

        OFloat val ->
            val

        ODate val ->
            val

        OTime val ->
            val

        NullOperand ->
            Nothing


constructor : Operand -> (Maybe String -> Operand)
constructor operand =
    case operand of
        OText _ ->
            text

        OInt _ ->
            int

        OFloat _ ->
            float

        ODate _ ->
            date

        OTime _ ->
            time

        NullOperand ->
            always NullOperand

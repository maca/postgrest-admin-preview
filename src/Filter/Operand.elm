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
    = OText String
    | OInt String
    | OFloat String
    | ODate String
    | OTime String


type Enum
    = Enum (List String) (Set String)


text : String -> Operand
text =
    OText


int : String -> Operand
int =
    OInt


float : String -> Operand
float =
    OFloat


date : String -> Operand
date =
    ODate


time : String -> Operand
time =
    OTime


enum : List String -> Set String -> Enum
enum =
    Enum


value : Operand -> String
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


rawValue : Operand -> String
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


constructor : Operand -> (String -> Operand)
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

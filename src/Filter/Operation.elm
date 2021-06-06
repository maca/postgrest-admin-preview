module Filter.Operation exposing (Operation(..), toPGQuery, toString)

import Filter.Operand as Operand exposing (Enum(..), Operand(..))
import Postgrest.Client as PG
import Set
import String.Extra as String


type Operation
    = IsTrue
    | IsFalse
    | IsNull (Maybe Operation)
    | Equals Operand
    | LesserThan Operand
    | GreaterThan Operand
    | Between Operand Operand
    | Contains Operand
    | StartsWith Operand
    | EndsWith Operand
    | InDate Operand
    | OneOf Enum
    | NoneOf Enum


type alias OperationConst =
    String -> String -> Operation


toPGQuery : String -> Operation -> Maybe PG.Param
toPGQuery name op =
    let
        param =
            PG.param name
    in
    case op of
        IsTrue ->
            Just <| param PG.true

        IsFalse ->
            Just <| param PG.false

        IsNull _ ->
            Just <| param PG.null

        Equals operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.map (param << PG.eq << PG.string)

        LesserThan operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.map (param << PG.lt << PG.string)

        GreaterThan operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.map (param << PG.gt << PG.string)

        Contains operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.map (\a -> param <| PG.ilike <| "*" ++ a ++ "*")

        StartsWith operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.map (\a -> param <| PG.ilike <| a ++ "*")

        EndsWith operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.map (\a -> param <| PG.ilike <| "*" ++ a)

        Between operandA operandB ->
            let
                makeOperation a b =
                    let
                        ( valA, valB ) =
                            case compare a b of
                                LT ->
                                    ( a, b )

                                _ ->
                                    ( b, a )
                    in
                    PG.and
                        [ PG.param name <| PG.gte <| PG.string valA
                        , PG.param name <| PG.lte <| PG.string valB
                        ]
            in
            Maybe.map2 makeOperation
                (String.nonEmpty <| Operand.value operandA)
                (String.nonEmpty <| Operand.value operandB)

        InDate operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.map (param << PG.eq << PG.string)

        OneOf (Enum _ chosen) ->
            if Set.isEmpty chosen then
                Just <| param PG.null

            else
                Just <| param <| PG.inList PG.string <| Set.toList chosen

        NoneOf (Enum choices chosen) ->
            if Set.isEmpty chosen then
                Just <| param <| PG.inList PG.string choices

            else
                Just <| param <| PG.not <| PG.inList PG.string <| Set.toList chosen


toString : Operation -> String
toString operation =
    case operation of
        IsTrue ->
            "is true"

        IsFalse ->
            "is false"

        IsNull _ ->
            "is not set"

        Equals _ ->
            "is equal to"

        LesserThan _ ->
            "is lesser than"

        GreaterThan _ ->
            "is greater than"

        Between _ _ ->
            "is between"

        Contains _ ->
            "contains"

        StartsWith _ ->
            "starts with"

        EndsWith _ ->
            "ends with"

        InDate _ ->
            "is in date"

        OneOf _ ->
            "is one of"

        NoneOf _ ->
            "is none of"

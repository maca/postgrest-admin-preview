module Filter.Operation exposing (Operation(..), toPGQuery, toString, values)

import Filter.Operand as Operand exposing (Enum, Operand(..))
import Postgrest.Client as PG
import Set
import String.Extra as String


type Operation
    = Equals Operand
    | LesserThan Operand
    | GreaterThan Operand
    | LesserOrEqual Operand
    | GreaterOrEqual Operand
    | Between Operand Operand
    | Contains Operand
    | StartsWith Operand
    | EndsWith Operand
    | InDate Operand
    | OneOf Enum
    | NoneOf Enum
    | IsTrue
    | IsFalse
    | IsInTheFuture (Maybe Operation)
    | IsInThePast (Maybe Operation)
    | IsNull (Maybe Operation)


toPGQuery : String -> Operation -> Maybe PG.Param
toPGQuery name op =
    let
        param =
            Just << PG.param name
    in
    case op of
        Equals operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.andThen (param << PG.eq << PG.string)

        LesserThan operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.andThen
                    (\s -> param <| PG.value <| PG.string <| "lt." ++ s)

        GreaterThan operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.andThen
                    (\s -> param <| PG.value <| PG.string <| "gt." ++ s)

        LesserOrEqual operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.andThen
                    (\s -> param <| PG.value <| PG.string <| "lte." ++ s)

        GreaterOrEqual operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.andThen
                    (\s -> param <| PG.value <| PG.string <| "gte." ++ s)

        Contains operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.andThen
                    (\a -> param <| PG.ilike <| "*" ++ a ++ "*")

        StartsWith operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.andThen (\a -> param <| PG.ilike <| a ++ "*")

        EndsWith operand ->
            Operand.value operand
                |> String.nonEmpty
                |> Maybe.andThen (\a -> param <| PG.ilike <| "*" ++ a)

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
                |> Maybe.andThen (param << PG.eq << PG.string)

        OneOf enum ->
            let
                chosen =
                    Operand.chosen enum
            in
            if Set.isEmpty chosen then
                param PG.null

            else
                param <| PG.inList PG.string <| Set.toList chosen

        NoneOf enum ->
            let
                choices =
                    Operand.choices enum

                chosen =
                    Operand.chosen enum
            in
            if Set.isEmpty chosen then
                param <| PG.inList PG.string choices

            else
                param <| PG.not (PG.inList PG.string <| Set.toList chosen)

        IsTrue ->
            param PG.true

        IsFalse ->
            param PG.false

        IsNull _ ->
            param PG.null

        IsInTheFuture _ ->
            param <| PG.value <| PG.string <| "gt.now"

        IsInThePast _ ->
            param <| PG.value <| PG.string <| "lt.now"


toString : Operation -> String
toString operation =
    case operation of
        Equals _ ->
            "is equal to"

        LesserThan _ ->
            "is lesser than"

        GreaterThan _ ->
            "is greater than"

        LesserOrEqual _ ->
            "is lesser or equal to"

        GreaterOrEqual _ ->
            "is greater or equal to"

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

        IsTrue ->
            "is true"

        IsFalse ->
            "is false"

        IsNull _ ->
            "is not set"

        IsInTheFuture _ ->
            "is in the future"

        IsInThePast _ ->
            "is in the past"


values : Operation -> List String
values operation =
    case operation of
        Equals a ->
            [ Operand.value a ]

        Contains a ->
            [ Operand.value a ]

        StartsWith a ->
            [ Operand.value a ]

        EndsWith a ->
            [ Operand.value a ]

        LesserThan a ->
            [ Operand.value a ]

        GreaterThan a ->
            [ Operand.value a ]

        LesserOrEqual a ->
            [ Operand.value a ]

        GreaterOrEqual a ->
            [ Operand.value a ]

        Between a b ->
            [ Operand.value a, Operand.value b ]

        InDate a ->
            [ Operand.value a ]

        OneOf enum ->
            Set.toList (Operand.chosen enum)

        NoneOf enum ->
            Set.diff (Set.fromList (Operand.choices enum)) (Operand.chosen enum) |> Set.toList

        IsTrue ->
            []

        IsFalse ->
            []

        IsNull _ ->
            []

        IsInTheFuture _ ->
            []

        IsInThePast _ ->
            []

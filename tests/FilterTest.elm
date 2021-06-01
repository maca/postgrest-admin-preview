module FilterTest exposing (..)

import Expect exposing (Expectation)
import Filter exposing (Filter(..))
import Filter.Operation as Operation exposing (Operation(..))
import Fuzz exposing (Fuzzer, int, list, string)
import Postgrest.Client as PG
import Test exposing (..)


suite : Test
suite =
    describe "Filter module"
        [ describe "Url codec"
            [ test "is.true" <|
                \_ ->
                    Filter.parseOp "is.true"
                        |> toQueryString
                        |> Expect.equal "foo=is.true"
            , test "IsTrue" <|
                \_ ->
                    Filter.parseOp "is.true" |> Expect.equal IsTrue
            , test "is.false" <|
                \_ ->
                    Filter.parseOp "is.false"
                        |> toQueryString
                        |> Expect.equal "foo=is.false"
            , test "IsFalse" <|
                \_ ->
                    Filter.parseOp "is.false" |> Expect.equal IsFalse
            , test "is.null" <|
                \_ ->
                    Filter.parseOp "is.null"
                        |> toQueryString
                        |> Expect.equal "foo=is.null"
            , test "IsNull" <|
                \_ ->
                    Filter.parseOp "is.null" |> Expect.equal IsNull
            , test "eq.bar%20baz" <|
                \_ ->
                    Filter.parseOp "eq.bar%20baz"
                        |> toQueryString
                        |> Expect.equal "foo=eq.bar%20baz"
            , test "Equals" <|
                \_ ->
                    Filter.parseOp "eq.bar%20baz"
                        |> Expect.equal (Equals <| Just "bar baz")
            , test "lt.1.1" <|
                \_ ->
                    Filter.parseOp "lt.1.1"
                        |> toQueryString
                        |> Expect.equal "foo=lt.\"1.1\""
            , test "LesserThan" <|
                \_ ->
                    Filter.parseOp "lt.1.1"
                        |> Expect.equal (LesserThan <| Just "1.1")
            , test "gt.1.1" <|
                \_ ->
                    Filter.parseOp "gt.1.1"
                        |> toQueryString
                        |> Expect.equal "foo=gt.\"1.1\""
            , test "GreaterThan" <|
                \_ ->
                    Filter.parseOp "gt.1.1"
                        |> Expect.equal (GreaterThan <| Just "1.1")
            , test "ilike.*bar%20baz*" <|
                \_ ->
                    Filter.parseOp "ilike.*bar%20baz*"
                        |> toQueryString
                        |> Expect.equal "foo=ilike.*bar%20baz*"
            , test "Contains" <|
                \_ ->
                    Filter.parseOp "ilike.*bar%20baz*"
                        |> Expect.equal (Contains <| Just "bar baz")
            , test "ilike.*bar%20baz" <|
                \_ ->
                    Filter.parseOp "ilike.*bar%20baz"
                        |> toQueryString
                        |> Expect.equal "foo=ilike.*bar%20baz"
            , test "StartsWith" <|
                \_ ->
                    Filter.parseOp "ilike.*bar%20baz"
                        |> Expect.equal (StartsWith <| Just "bar baz")
            , test "ilike.bar%20baz*" <|
                \_ ->
                    Filter.parseOp "ilike.bar%20baz*"
                        |> toQueryString
                        |> Expect.equal "foo=ilike.bar%20baz*"
            , test "EndsWith" <|
                \_ ->
                    Filter.parseOp "ilike.bar%20baz*"
                        |> Expect.equal (EndsWith <| Just "bar baz")

            -- , test "ilike.bar%20baz*" <|
            --     \_ ->
            --         Filter.parseOp "ilike.bar%20baz*"
            --             |> toQueryString
            --             |> Expect.equal "foo=ilike.bar%20baz*"
            -- , test "EndsWith" <|
            --     \_ ->
            --         Filter.parseOp "ilike.bar%20baz*"
            --             |> Expect.equal (EndsWith <| Just "bar baz")
            ]
        ]


toQueryString =
    Operation.toPGQuery "foo"
        >> List.singleton
        >> List.filterMap identity
        >> PG.toQueryString

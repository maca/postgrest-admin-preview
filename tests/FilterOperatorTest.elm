module FilterOperatorTest exposing (..)

import Expect exposing (Expectation)
import Filter.Operator as Operator exposing (Operator(..))
import Fuzz exposing (Fuzzer, int, list, string)
import Postgrest.Client as PG
import Test exposing (..)


suite : Test
suite =
    describe "Filter module"
        [ describe "Url codec"
            [ test "is.true" <|
                \_ ->
                    Operator.parse "is.true"
                        |> toQueryString
                        |> Expect.equal "foo=is.true"
            , test "IsTrue" <|
                \_ ->
                    Operator.parse "is.true" |> Expect.equal IsTrue
            , test "is.false" <|
                \_ ->
                    Operator.parse "is.false"
                        |> toQueryString
                        |> Expect.equal "foo=is.false"
            , test "IsFalse" <|
                \_ ->
                    Operator.parse "is.false" |> Expect.equal IsFalse
            , test "is.null" <|
                \_ ->
                    Operator.parse "is.null"
                        |> toQueryString
                        |> Expect.equal "foo=is.null"
            , test "IsNull" <|
                \_ ->
                    Operator.parse "is.null" |> Expect.equal IsNull
            , test "eq.bar%20baz" <|
                \_ ->
                    Operator.parse "eq.bar%20baz"
                        |> toQueryString
                        |> Expect.equal "foo=eq.bar%20baz"
            , test "Equals" <|
                \_ ->
                    Operator.parse "eq.bar%20baz"
                        |> Expect.equal (Equals <| Just "bar baz")
            , test "lt.1.1" <|
                \_ ->
                    Operator.parse "lt.1.1"
                        |> toQueryString
                        |> Expect.equal "foo=lt.\"1.1\""
            , test "LesserThan" <|
                \_ ->
                    Operator.parse "lt.1.1"
                        |> Expect.equal (LesserThan <| Just "1.1")
            , test "gt.1.1" <|
                \_ ->
                    Operator.parse "gt.1.1"
                        |> toQueryString
                        |> Expect.equal "foo=gt.\"1.1\""
            , test "GreaterThan" <|
                \_ ->
                    Operator.parse "gt.1.1"
                        |> Expect.equal (GreaterThan <| Just "1.1")

            -- , test "ilike.*bar%20baz*" <|
            --     \_ ->
            --         Operator.parse "ilike.*bar%20baz*"
            --             |> toQueryString
            --             |> Expect.equal "foo=ilike.*bar%20baz*"
            -- , test "Contains" <|
            --     \_ ->
            --         Operator.parse "ilike.*bar%20baz*"
            --             |> Expect.equal (Contains <| Just "bar baz")
            -- , test "ilike.*bar%20baz" <|
            --     \_ ->
            --         Operator.parse "ilike.*bar%20baz"
            --             |> toQueryString
            --             |> Expect.equal "foo=ilike.*bar%20baz"
            -- , test "StartsWith" <|
            --     \_ ->
            --         Operator.parse "ilike.*bar%20baz"
            --             |> Expect.equal (StartsWith <| Just "bar baz")
            -- , test "ilike.bar%20baz*" <|
            --     \_ ->
            --         Operator.parse "ilike.bar%20baz*"
            --             |> toQueryString
            --             |> Expect.equal "foo=ilike.bar%20baz*"
            -- , test "EndsWith" <|
            --     \_ ->
            --         Operator.parse "ilike.bar%20baz*"
            --             |> Expect.equal (EndsWith <| Just "bar baz")
            ]
        ]


toQueryString =
    Operator.toPGQuery "foo"
        >> List.singleton
        >> List.filterMap identity
        >> PG.toQueryString

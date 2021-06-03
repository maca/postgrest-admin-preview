module FilterTest exposing (..)

import Dict
import Expect exposing (Expectation)
import Filter exposing (Filter(..))
import Filter.Operation as Operation exposing (Operation(..))
import Fuzz exposing (Fuzzer, int, list, string)
import Postgrest.Client as PG
import Postgrest.Schema.Definition as Definition exposing (Column(..), Definition)
import Postgrest.Value exposing (Value(..))
import Test exposing (..)


suite : Test
suite =
    describe "Filter module"
        [ describe "Url codec"
            [ test "is.true" <|
                \_ ->
                    Filter.parse definition "bool=is.true"
                        |> toQueryString
                        |> Expect.equal "bool=is.true"
            , test "IsTrue" <|
                \_ ->
                    Filter.parse definition "bool=is.true"
                        |> Expect.equal (Just <| BoolFilter "bool" IsTrue)
            , test "is.false" <|
                \_ ->
                    Filter.parse definition "bool=is.false"
                        |> toQueryString
                        |> Expect.equal "bool=is.false"
            , test "IsFalse" <|
                \_ ->
                    Filter.parse definition "bool=is.false"
                        |> Expect.equal (Just <| BoolFilter "bool" IsFalse)
            , test "is.null" <|
                \_ ->
                    Filter.parse definition "bool=is.null"
                        |> toQueryString
                        |> Expect.equal "bool=is.null"
            , test "IsNull" <|
                \_ ->
                    Filter.parse definition "bool=is.null"
                        |> Expect.equal (Just <| BoolFilter "bool" IsNull)
            , test "eq.bar%20baz" <|
                \_ ->
                    Filter.parse definition "text=eq.bar%20baz"
                        |> toQueryString
                        |> Expect.equal "text=eq.bar%20baz"
            , test "Equals" <|
                \_ ->
                    Filter.parse definition "text=eq.bar%20baz"
                        |> Expect.equal
                            (Just "bar baz"
                                |> Equals
                                |> TextFilter "text"
                                |> Just
                            )
            , test "ilike.*bar%20baz*" <|
                \_ ->
                    Filter.parse definition "text=ilike.*bar%20baz*"
                        |> toQueryString
                        |> Expect.equal "text=ilike.*bar%20baz*"
            , test "Contains" <|
                \_ ->
                    Filter.parse definition "text=ilike.*bar%20baz*"
                        |> Expect.equal
                            (Just "bar baz"
                                |> Contains
                                |> TextFilter "text"
                                |> Just
                            )
            , test "ilike.bar%20baz*" <|
                \_ ->
                    Filter.parse definition "text=ilike.bar%20baz*"
                        |> toQueryString
                        |> Expect.equal "text=ilike.bar%20baz*"
            , test "StartsWith" <|
                \_ ->
                    Filter.parse definition "text=ilike.bar%20baz*"
                        |> Expect.equal
                            (Just "bar baz"
                                |> StartsWith
                                |> TextFilter "text"
                                |> Just
                            )
            , test "ilike.*bar%20baz" <|
                \_ ->
                    Filter.parse definition "text=ilike.*bar%20baz"
                        |> toQueryString
                        |> Expect.equal "text=ilike.*bar%20baz"
            , test "EndsWith" <|
                \_ ->
                    Filter.parse definition "text=ilike.*bar%20baz"
                        |> Expect.equal
                            (Just "bar baz"
                                |> EndsWith
                                |> TextFilter "text"
                                |> Just
                            )
            , test "lt.1.1" <|
                \_ ->
                    Filter.parse definition "float=lt.\"1.1\""
                        |> toQueryString
                        |> Expect.equal "float=lt.\"1.1\""
            , test "LesserThan" <|
                \_ ->
                    Filter.parse definition "float=lt.\"1.1\""
                        |> Expect.equal
                            (Just "1.1"
                                |> LesserThan
                                |> FloatFilter "float"
                                |> Just
                            )
            , test "gt.1.1" <|
                \_ ->
                    Filter.parse definition "float=gt.\"1.1\""
                        |> toQueryString
                        |> Expect.equal "float=gt.\"1.1\""
            , test "GreaterThan" <|
                \_ ->
                    Filter.parse definition "float=gt.\"1.1\""
                        |> Expect.equal
                            (Just "1.1"
                                |> GreaterThan
                                |> FloatFilter "float"
                                |> Just
                            )
            ]
        ]


toQueryString mfilter =
    case mfilter of
        Just filter ->
            Filter.toPGQuery filter
                |> List.singleton
                |> List.filterMap identity
                |> PG.toQueryString

        Nothing ->
            ""


definition : Definition
definition =
    Dict.fromList
        [ ( "float", Column False <| PFloat Nothing )
        , ( "int", Column False <| PInt Nothing )
        , ( "string", Column False <| PInt Nothing )
        , ( "text", Column False <| PText Nothing )
        , ( "enum", Column False <| PEnum Nothing [] )
        , ( "bool", Column False <| PBool Nothing )
        , ( "time", Column False <| PTime Nothing )
        , ( "date", Column False <| PDate Nothing )
        ]

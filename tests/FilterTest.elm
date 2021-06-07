module FilterTest exposing (..)

import Dict
import Expect exposing (Expectation)
import Filter
    exposing
        ( Filter
        , between
        , columnName
        , contains
        , date
        , endsWith
        , equals
        , float
        , fromColumn
        , greaterThan
        , inDate
        , init
        , int
        , isFalse
        , isNull
        , isTrue
        , lesserThan
        , noneOf
        , oneOf
        , operation
        , parse
        , startsWith
        , text
        , time
        )
import Fuzz exposing (Fuzzer, int, list, string)
import Postgrest.Client as PG
import Postgrest.Schema.Definition as Definition exposing (Column(..), Definition)
import Postgrest.Value exposing (Value(..))
import Test exposing (..)


suite : Test
suite =
    describe "Filter"
        [ test "codec" <|
            \_ ->
                Expect.all
                    ([ "bool=is.true"
                     , "bool=is.false"
                     , "bool=is.null"
                     , "text=eq.bar%20baz"
                     , "text=ilike.*bar%20baz*"
                     , "text=ilike.bar%20baz*"
                     , "text=ilike.*bar%20baz"
                     , "float=lt.1.1"
                     , "float=gt.1.1"
                     ]
                        |> List.map
                            (\q ->
                                parse definition q
                                    |> Maybe.map Filter.toQueryString
                                    |> Expect.equal (Just q)
                                    |> always
                            )
                    )
                    ()
        , describe "parse and construct"
            [ test "is.true" <|
                \_ ->
                    parse definition "bool=is.true"
                        |> Expect.equal (Just <| isTrue "bool")
            , test "is.false" <|
                \_ ->
                    parse definition "bool=is.false"
                        |> Expect.equal (Just <| isFalse "bool")
            , test "is.null" <|
                \_ ->
                    parse definition "bool=is.null"
                        |> Expect.equal (Just <| isNull "bool")
            , test "equals" <|
                \_ ->
                    parse definition "text=eq.bar%20baz"
                        |> Expect.equal (Just <| text "text" equals "bar baz")
            , test "contains" <|
                \_ ->
                    Filter.parse definition "text=ilike.*bar%20baz*"
                        |> Expect.equal (Just <| text "text" contains "bar baz")
            , test "starts with" <|
                \_ ->
                    Filter.parse definition "text=ilike.bar%20baz*"
                        |> Expect.equal
                            (Just <| text "text" startsWith "bar baz")
            , test "ends with" <|
                \_ ->
                    Filter.parse definition "text=ilike.*bar%20baz"
                        |> Expect.equal (Just <| text "text" endsWith "bar baz")
            , test "lesser than" <|
                \_ ->
                    Filter.parse definition "float=lt.1.1"
                        |> Expect.equal (Just <| float "float" lesserThan "1.1")
            , test "greater than" <|
                \_ ->
                    Filter.parse definition "float=gt.1.1"
                        |> Expect.equal
                            (Just <| float "float" greaterThan "1.1")
            ]
        ]


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

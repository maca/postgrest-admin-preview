module FilterTest exposing (suite)

import Dict
import Expect
import Internal.Filter as Filter exposing (..)
import Internal.Schema as Table
    exposing
        ( Table
        )
import Internal.Value exposing (Value(..))
import Json.Decode
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
                     , "int=lt.1"
                     , "int=gt.1"
                     , "float=lt.1.1"
                     , "float=gt.1.1"
                     , "date=gt.now"
                     , "date=lt.now"
                     , "time=gt.now"
                     , "time=lt.now"
                     , "int=lte.1"
                     , "int=gte.1"
                     , "float=lte.1.1"
                     , "float=gte.1.1"
                     , "date=gte.now"
                     , "date=lte.now"
                     , "time=gte.now"
                     , "time=lte.now"
                     , "and=(int.gte.1,int.lte.2)"
                     , "and=(float.gte.1.1,float.lte.2.2)"
                     , "and=(date.gte.1800-01-01,date.lte.2021-01-02)"
                     , "and=(time.gte.2021-01-02T01%3A10,time.lte.2027-01-03T01%3A00)"
                     ]
                        |> List.map
                            (\q ->
                                parse table q
                                    |> Maybe.map Filter.toQueryString
                                    |> Expect.equal (Just q)
                                    |> always
                            )
                    )
                    ()
        , describe "parse and construct"
            [ test "is.true" <|
                \_ ->
                    parse table "bool=is.true"
                        |> Expect.equal (Just <| isTrue "bool")
            , test "is.false" <|
                \_ ->
                    parse table "bool=is.false"
                        |> Expect.equal (Just <| isFalse "bool")
            , test "is.null" <|
                \_ ->
                    parse table "bool=is.null"
                        |> Expect.equal (Just <| isNull "bool")
            , test "equals" <|
                \_ ->
                    parse table "text=eq.bar%20baz"
                        |> Expect.equal (Just <| text "text" equals "bar baz")
            , test "contains" <|
                \_ ->
                    Filter.parse table "text=ilike.*bar%20baz*"
                        |> Expect.equal (Just <| text "text" contains "bar baz")
            , test "starts with" <|
                \_ ->
                    Filter.parse table "text=ilike.bar%20baz*"
                        |> Expect.equal
                            (Just <| text "text" startsWith "bar baz")
            , test "ends with" <|
                \_ ->
                    Filter.parse table "text=ilike.*bar%20baz"
                        |> Expect.equal (Just <| text "text" endsWith "bar baz")
            , test "lesser than" <|
                \_ ->
                    Filter.parse table "float=lt.1.1"
                        |> Expect.equal (Just <| float "float" lesserThan "1.1")
            , test "greater than" <|
                \_ ->
                    Filter.parse table "float=gt.1.1"
                        |> Expect.equal
                            (Just <| float "float" greaterThan "1.1")
            , test "lesser or equal to" <|
                \_ ->
                    Filter.parse table "float=lte.1.1"
                        |> Expect.equal
                            (Just <| float "float" lesserOrEqual "1.1")
            , test "greater or equal to" <|
                \_ ->
                    Filter.parse table "float=gte.1.1"
                        |> Expect.equal
                            (Just <| float "float" greaterOrEqual "1.1")
            , test "between int" <|
                \_ ->
                    Filter.parse table
                        "and=(int.gte.1,int.lte.10)"
                        |> Expect.equal
                            (Just <| int2 "int" between "1" "10")
            , test "between float" <|
                \_ ->
                    Filter.parse table
                        "and=(float.gte.1.1,float.lte.10.1)"
                        |> Expect.equal
                            (Just <| float2 "float" between "1.1" "10.1")
            , test "between date" <|
                \_ ->
                    Filter.parse table
                        "and=(date.gte.1800-01-01,date.lte.2021-01-02)"
                        |> Expect.equal
                            (Just <|
                                date2 "date" between "1800-01-01" "2021-01-02"
                            )
            , test "between time" <|
                \_ ->
                    Filter.parse table
                        "and=(time.gte.1800-01-01,time.lte.2021-01-02)"
                        |> Expect.equal
                            (Just <|
                                time2 "time" between "1800-01-01" "2021-01-02"
                            )
            , test "between time inverted" <|
                \_ ->
                    Filter.parse table
                        "and=(time.lte.2021-01-02,time.gte.1800-01-01)"
                        |> Expect.equal
                            (Just <|
                                time2 "time" between "1800-01-01" "2021-01-02"
                            )
            , test "time in the future" <|
                \_ ->
                    Filter.parse table "time=gt.now"
                        |> Expect.equal (Just <| isInTheFuture "time")
            , test "time in the past" <|
                \_ ->
                    Filter.parse table "time=lt.now"
                        |> Expect.equal
                            (Just <| isInThePast "time")
            , test "date in the future" <|
                \_ ->
                    Filter.parse table "date=gt.now"
                        |> Expect.equal (Just <| isInTheFuture "date")
            , test "date in the past" <|
                \_ ->
                    Filter.parse table "date=lt.now"
                        |> Expect.equal
                            (Just <| isInThePast "date")
            , test "time is in date" <|
                \_ ->
                    let
                        times =
                            String.join ","
                                [ "time.gte.2021-01-01T00%3A00"
                                , "time.lt.2021-01-02T00%3A00"
                                ]

                        query =
                            "and=(" ++ times ++ ")"
                    in
                    Filter.parse table query
                        |> Expect.equal
                            (Just <| time "time" inDate "2021-01-01")
            , test "time is in date inverted" <|
                \_ ->
                    let
                        times =
                            String.join ","
                                [ "time.lt.2021-01-02T00%3A00"
                                , "time.gte.2021-01-01T00%3A00"
                                ]

                        query =
                            "and=(" ++ times ++ ")"
                    in
                    Filter.parse table query
                        |> Expect.equal
                            (Just <| time "time" inDate "2021-01-01")
            ]
        ]


table : Table
table =
    let
        column val =
            { constraint = Table.NoConstraint
            , required = False
            , value = val
            , decoder = Json.Decode.succeed val
            }
    in
    { name = "test_table"
    , columns =
        Dict.fromList
            [ ( "float", column <| PFloat Nothing )
            , ( "int", column <| PInt Nothing )
            , ( "string", column <| PInt Nothing )
            , ( "text", column <| PText Nothing )
            , ( "enum", column <| PEnum Nothing [] )
            , ( "bool", column <| PBool Nothing )
            , ( "time", column <| PTime Nothing )
            , ( "date", column <| PDate Nothing )
            ]
    }

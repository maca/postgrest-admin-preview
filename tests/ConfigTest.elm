module ConfigTest exposing (suite)

import Dict
import Expect
import Http
import Internal.Config
import Json.Encode as Encode
import PostgRestAdmin.Client as Client
import PostgRestAdmin.Config as Config
import PostgRestAdmin.MountPath as MountPath
import Test exposing (..)
import Url


suite : Test
suite =
    describe "PostgRestAdmin Config and Flags"
        [ hostTests
        , mountPathTests
        , loginUrlTests
        , tablesTests
        , tableAliasesTests
        , formFieldsTests
        , menuLinksTests
        , clientHeadersTests
        , jwtTests
        ]



-- HOST TESTS


hostTests : Test
hostTests =
    describe "host configuration"
        [ test "default host is http://localhost:3000" <|
            \_ ->
                Internal.Config.decode
                    Config.init
                    (Encode.object [])
                    |> Result.map (.host >> Url.toString)
                    |> Expect.equal (Ok "http://localhost:3000")
        , test "Config.host sets the host" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.host "http://localhost:9080"
                    )
                    (Encode.object [])
                    |> Result.map (.host >> Url.toString)
                    |> Expect.equal (Ok "http://localhost:9080/")
        , test "host flag sets the host" <|
            \_ ->
                Internal.Config.decode
                    Config.init
                    (Encode.object
                        [ ( "host", Encode.string "http://localhost:9080" )
                        ]
                    )
                    |> Result.map (.host >> Url.toString)
                    |> Expect.equal (Ok "http://localhost:9080/")
        , test "host flag takes precedence over Config.host" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.host "http://localhost:3000"
                    )
                    (Encode.object
                        [ ( "host", Encode.string "http://localhost:9080" )
                        ]
                    )
                    |> Result.map (.host >> Url.toString)
                    |> Expect.equal (Ok "http://localhost:9080/")
        ]



-- LOGIN URL TESTS


loginUrlTests : Test
loginUrlTests =
    describe "loginUrl configuration"
        [ test "default loginUrl is http://localhost:3000/rpc/login" <|
            \_ ->
                Internal.Config.decode
                    Config.init
                    (Encode.object [])
                    |> Result.map (.loginUrl >> Url.toString)
                    |> Expect.equal (Ok "http://localhost:3000/rpc/login")
        , test "loginUrl derives from host" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.host "http://localhost:9080"
                    )
                    (Encode.object [])
                    |> Result.map (.loginUrl >> Url.toString)
                    |> Expect.equal (Ok "http://localhost:9080/rpc/login")
        , test "Config.loginUrl overrides derived loginUrl" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.host "http://localhost:9080"
                        |> Config.loginUrl "http://localhost:9080/rpc/authenticate"
                    )
                    (Encode.object [])
                    |> Result.map (.loginUrl >> Url.toString)
                    |> Expect.equal (Ok "http://localhost:9080/rpc/authenticate")
        , test "Config.loginUrl can use different host than Config.host" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.host "http://localhost:3000"
                        |> Config.loginUrl "http://auth.example.com/login"
                    )
                    (Encode.object [])
                    |> Result.map (.loginUrl >> Url.toString)
                    |> Expect.equal (Ok "http://auth.example.com/login")
        , test "loginUrl flag sets the login URL" <|
            \_ ->
                Internal.Config.decode
                    Config.init
                    (Encode.object
                        [ ( "loginUrl", Encode.string "http://localhost:9080/rpc/login" )
                        ]
                    )
                    |> Result.map (.loginUrl >> Url.toString)
                    |> Expect.equal (Ok "http://localhost:9080/rpc/login")
        , test "loginUrl flag takes precedence over Config.loginUrl" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.loginUrl "http://localhost:3000/rpc/login"
                    )
                    (Encode.object
                        [ ( "loginUrl", Encode.string "http://localhost:9080/rpc/login" )
                        ]
                    )
                    |> Result.map (.loginUrl >> Url.toString)
                    |> Expect.equal (Ok "http://localhost:9080/rpc/login")
        ]



-- MOUNT PATH TESTS


mountPathTests : Test
mountPathTests =
    describe "mountPath configuration"
        [ test "default mountPath is empty string" <|
            \_ ->
                Internal.Config.decode
                    Config.init
                    (Encode.object [])
                    |> Result.map .mountPath
                    |> Expect.equal (Ok (MountPath.fromString ""))
        , test "Config.mountPath sets the mount path" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.mountPath "/admin"
                    )
                    (Encode.object [])
                    |> Result.map .mountPath
                    |> Expect.equal (Ok (MountPath.fromString "/admin"))
        , test "mountPath flag sets the mount path" <|
            \_ ->
                Internal.Config.decode
                    Config.init
                    (Encode.object
                        [ ( "mountPath", Encode.string "/admin" )
                        ]
                    )
                    |> Result.map .mountPath
                    |> Expect.equal (Ok (MountPath.fromString "/admin"))
        , test "mountPath flag takes precedence over Config.mountPath" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.mountPath "/dashboard"
                    )
                    (Encode.object
                        [ ( "mountPath", Encode.string "/admin" )
                        ]
                    )
                    |> Result.map .mountPath
                    |> Expect.equal (Ok (MountPath.fromString "/admin"))
        ]



-- TABLES TESTS


tablesTests : Test
tablesTests =
    describe "tables configuration"
        [ test "default tables is empty list" <|
            \_ ->
                Internal.Config.decode
                    Config.init
                    (Encode.object [])
                    |> Result.map .tables
                    |> Expect.equal (Ok [])
        , test "Config.tables sets the tables list" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.tables [ "users", "posts" ]
                    )
                    (Encode.object [])
                    |> Result.map .tables
                    |> Expect.equal (Ok [ "users", "posts" ])
        , test "tables flag sets the tables list" <|
            \_ ->
                Internal.Config.decode
                    Config.init
                    (Encode.object
                        [ ( "tables"
                          , Encode.list Encode.string [ "users", "posts" ]
                          )
                        ]
                    )
                    |> Result.map .tables
                    |> Expect.equal (Ok [ "users", "posts" ])
        , test "tables flag takes precedence over Config.tables" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.tables [ "comments", "tags" ]
                    )
                    (Encode.object
                        [ ( "tables"
                          , Encode.list Encode.string [ "users", "posts" ]
                          )
                        ]
                    )
                    |> Result.map .tables
                    |> Expect.equal (Ok [ "users", "posts" ])
        ]



-- TABLE ALIASES TESTS


tableAliasesTests : Test
tableAliasesTests =
    describe "tableAliases configuration"
        [ test "default tableAliases is empty dict" <|
            \_ ->
                Internal.Config.decode
                    Config.init
                    (Encode.object [])
                    |> Result.map .tableAliases
                    |> Expect.equal (Ok Dict.empty)
        , test "Config.tableAliases sets the table aliases" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.tableAliases
                            (Dict.fromList
                                [ ( "published_posts", "posts" )
                                ]
                            )
                    )
                    (Encode.object [])
                    |> Result.map .tableAliases
                    |> Expect.equal
                        (Ok (Dict.fromList [ ( "published_posts", "posts" ) ]))
        , test "tableAliases flag sets the table aliases" <|
            \_ ->
                Internal.Config.decode
                    Config.init
                    (Encode.object
                        [ ( "tableAliases"
                          , Encode.object
                                [ ( "published_posts", Encode.string "posts" )
                                ]
                          )
                        ]
                    )
                    |> Result.map .tableAliases
                    |> Expect.equal
                        (Ok (Dict.fromList [ ( "published_posts", "posts" ) ]))
        , test "tableAliases flag takes precedence over Config.tableAliases" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.tableAliases
                            (Dict.fromList
                                [ ( "active_users", "users" )
                                ]
                            )
                    )
                    (Encode.object
                        [ ( "tableAliases"
                          , Encode.object
                                [ ( "published_posts", Encode.string "posts" )
                                ]
                          )
                        ]
                    )
                    |> Result.map .tableAliases
                    |> Expect.equal
                        (Ok (Dict.fromList [ ( "published_posts", "posts" ) ]))
        ]



-- FORM FIELDS TESTS


formFieldsTests : Test
formFieldsTests =
    describe "formFields configuration"
        [ test "default formFields is empty dict" <|
            \_ ->
                Internal.Config.decode
                    Config.init
                    (Encode.object [])
                    |> Result.map .formFields
                    |> Expect.equal (Ok Dict.empty)
        , test "Config.formFields sets the form fields" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.formFields "users" [ "id", "name", "email" ]
                    )
                    (Encode.object [])
                    |> Result.map (.formFields >> Dict.get "users")
                    |> Expect.equal (Ok (Just [ "id", "name", "email" ]))
        , test "formFields flag sets the form fields" <|
            \_ ->
                Internal.Config.decode
                    Config.init
                    (Encode.object
                        [ ( "formFields"
                          , Encode.object
                                [ ( "users"
                                  , Encode.list Encode.string
                                        [ "id", "name", "email" ]
                                  )
                                ]
                          )
                        ]
                    )
                    |> Result.map (.formFields >> Dict.get "users")
                    |> Expect.equal (Ok (Just [ "id", "name", "email" ]))
        , test "formFields flag takes precedence over Config.formFields" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.formFields "users" [ "id", "username" ]
                    )
                    (Encode.object
                        [ ( "formFields"
                          , Encode.object
                                [ ( "users"
                                  , Encode.list Encode.string
                                        [ "id", "name", "email" ]
                                  )
                                ]
                          )
                        ]
                    )
                    |> Result.map (.formFields >> Dict.get "users")
                    |> Expect.equal (Ok (Just [ "id", "name", "email" ]))
        ]



-- MENU LINKS TESTS


menuLinksTests : Test
menuLinksTests =
    describe "menuLinks configuration"
        [ test "default menuLinks is empty list" <|
            \_ ->
                Internal.Config.decode
                    Config.init
                    (Encode.object [])
                    |> Result.map .menuLinks
                    |> Expect.equal (Ok [])
        , test "Config.menuLinks sets the menu links" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.menuLinks
                            [ ( "Documentation", "/docs" )
                            , ( "API", "/api" )
                            ]
                    )
                    (Encode.object [])
                    |> Result.map .menuLinks
                    |> Expect.equal
                        (Ok
                            [ ( "Documentation", "/docs" )
                            , ( "API", "/api" )
                            ]
                        )
        , test "menuLinks flag sets the menu links" <|
            \_ ->
                Internal.Config.decode
                    Config.init
                    (Encode.object
                        [ ( "menuLinks"
                          , Encode.list
                                (\( text, url ) ->
                                    Encode.object
                                        [ ( "text", Encode.string text )
                                        , ( "url", Encode.string url )
                                        ]
                                )
                                [ ( "Documentation", "/docs" )
                                , ( "API", "/api" )
                                ]
                          )
                        ]
                    )
                    |> Result.map .menuLinks
                    |> Expect.equal
                        (Ok
                            [ ( "Documentation", "/docs" )
                            , ( "API", "/api" )
                            ]
                        )
        , test "menuLinks flag takes precedence over Config.menuLinks" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.menuLinks
                            [ ( "Home", "/" )
                            ]
                    )
                    (Encode.object
                        [ ( "menuLinks"
                          , Encode.list
                                (\( text, url ) ->
                                    Encode.object
                                        [ ( "text", Encode.string text )
                                        , ( "url", Encode.string url )
                                        ]
                                )
                                [ ( "Documentation", "/docs" )
                                , ( "API", "/api" )
                                ]
                          )
                        ]
                    )
                    |> Result.map .menuLinks
                    |> Expect.equal
                        (Ok
                            [ ( "Documentation", "/docs" )
                            , ( "API", "/api" )
                            ]
                        )
        ]



-- CLIENT HEADERS TESTS


clientHeadersTests : Test
clientHeadersTests =
    describe "clientHeaders configuration"
        [ test "default clientHeaders is empty list" <|
            \_ ->
                Internal.Config.decode
                    Config.init
                    (Encode.object [])
                    |> Result.map .clientHeaders
                    |> Expect.equal (Ok [])
        , test "Config.clientHeaders sets the client headers" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.clientHeaders
                            [ Http.header "Accept-Profile" "bluebox"
                            , Http.header "Content-Profile" "bluebox"
                            ]
                    )
                    (Encode.object [])
                    |> Result.map (.clientHeaders >> List.length)
                    |> Expect.equal (Ok 2)
        , test "clientHeaders flag sets the client headers" <|
            \_ ->
                Internal.Config.decode
                    Config.init
                    (Encode.object
                        [ ( "clientHeaders"
                          , Encode.object
                                [ ( "Accept-Profile", Encode.string "bluebox" )
                                , ( "Content-Profile", Encode.string "bluebox" )
                                ]
                          )
                        ]
                    )
                    |> Result.map (.clientHeaders >> List.length)
                    |> Expect.equal (Ok 2)
        , test "clientHeaders flag takes precedence over Config.clientHeaders" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.clientHeaders
                            [ Http.header "Accept-Profile" "public"
                            , Http.header "Content-Profile" "public"
                            ]
                    )
                    (Encode.object
                        [ ( "clientHeaders"
                          , Encode.object
                                [ ( "Accept-Profile", Encode.string "bluebox" )
                                ]
                          )
                        ]
                    )
                    |> Result.map (.clientHeaders >> List.length)
                    |> Expect.equal (Ok 1)
        ]



-- JWT TESTS


jwtTests : Test
jwtTests =
    describe "jwt configuration"
        [ test "default authScheme is unset" <|
            \_ ->
                Internal.Config.decode
                    Config.init
                    (Encode.object [])
                    |> Result.map .authScheme
                    |> Expect.equal (Ok Client.unset)
        , test "Config.jwt sets the JWT token in authScheme" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.jwt "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"
                    )
                    (Encode.object [])
                    |> Result.map .authScheme
                    |> Expect.equal
                        (Ok (Client.jwt "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"))
        , test "jwt flag sets the JWT token in authScheme" <|
            \_ ->
                Internal.Config.decode
                    Config.init
                    (Encode.object
                        [ ( "jwt", Encode.string "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9" )
                        ]
                    )
                    |> Result.map .authScheme
                    |> Expect.equal
                        (Ok (Client.jwt "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"))
        , test "jwt flag takes precedence over Config.jwt" <|
            \_ ->
                Internal.Config.decode
                    (Config.init
                        |> Config.jwt "old-token"
                    )
                    (Encode.object
                        [ ( "jwt", Encode.string "new-token-from-flag" )
                        ]
                    )
                    |> Result.map .authScheme
                    |> Expect.equal
                        (Ok (Client.jwt "new-token-from-flag"))
        ]

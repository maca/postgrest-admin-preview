module PostgRestAdmin.Config.FormAuth exposing
    ( FormAuth
    , config
    , authUrl
    , encoder
    , decoder
    )

{-| Configuration for form based authentication.

Tipically user credentials are exchanged for a
[JWT](https://en.wikipedia.org/wiki/JSON_Web_Token), to the PostgREST instance
or to an external authentication provided.

This module provides configuration functions for defining what is the
authentication form POST url, how the credentials are to be encoded and how the
JWT is to be decoded.
`FormAuth` configurations are to be used with
[Config.formAuth](PostgRestAdmin.Config#formAuth).

See
[PostgREST documentation](https://postgrest.org/en/stable/auth.html?highlight=authentication#)
to get a better understanding of JWT and roles in PostgREST.


# Config

@docs FormAuth
@docs config


# Host

@docs authUrl


# Encode/Decode

@docs encoder
@docs decoder

-}

import Dict exposing (Dict)
import Internal.Client as Client
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


{-| Basic authentication configuration.
-}
type alias FormAuth =
    Decoder Client.AuthScheme


{-| Create a authentication configuration.
-}
config : FormAuth
config =
    Client.authSchemeConfig


{-| Set authentication request login url. Credentials are to be exchanged for a
JWT via a post request.

    authUrl "http://localhost:3000/rpc/login" config

Alternatively the host can be specified using flags, configuring using
`authUrl`. Program flags take precedence.

    Elm.Main.init
        { flags = { authUrl = "http://localhost:3000/rpc/login" }
        }

-}
authUrl : String -> FormAuth -> FormAuth
authUrl =
    Client.authUrl


{-| Override the credentials JSON encoder to be used when posting to the login
url.

    encoder
        (\creds ->
            Encode.object
                [ ( "credentials"
                  , Encode.dict identity Encode.string creds
                  )
                ]
        )
        config

-}
encoder :
    (Dict String String -> Value)
    -> FormAuth
    -> FormAuth
encoder =
    Client.encoder


{-| Override the JSON decoder used to obtain the JWT from the login response.

    decoder (Decode.at [ "auth", "jwt" ] Decode.string) config

-}
decoder : Decoder String -> FormAuth -> FormAuth
decoder =
    Client.decoder

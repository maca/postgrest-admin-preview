module PostgRestAdmin.Config.FormAuth exposing
    ( FormAuth
    , config
    , withAuthUrl
    , withEncoder
    , withDecoder
    )

{-| Configuration for form based authentication.

Tipically user credentials are exchanged for a
[JWT](https://en.wikipedia.org/wiki/JSON_Web_Token), to the PostgREST instance
or to an external authentication provided.

This module provides configuration functions for defining what is the
authentication form POST url, how the credentials are to be encoded and how the
JWT is to be decoded.

See
[PostgREST documentation](https://postgrest.org/en/stable/auth.html?highlight=authentication#)
to get a better understanding of JWT and roles in PostgREST.


# Config

@docs FormAuth
@docs config


# Host

@docs withAuthUrl


# Encode/Decode

@docs withEncoder
@docs withDecoder

-}

import Dict exposing (Dict)
import Internal.FormAuth as FormAuth
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


{-| Basic authentication configuration.
-}
type alias FormAuth =
    Decoder FormAuth.FormAuth


{-| Create a authentication configuration.
-}
config : FormAuth
config =
    FormAuth.config


{-| Set authentication request login url. Credentials are to be exchanged for a
JWT via a post request.

      config
          |> withAuthUrl "http://localhost:3000/rpc/login"

Alternatively the host can be specified using flags, configuring using
`withAuthUrl`. Program flags take precedence.

      Elm.Main.init({
          flags: { authUrl: "http://localhost:3000/rpc/login" }
      })

-}
withAuthUrl : String -> FormAuth -> FormAuth
withAuthUrl =
    FormAuth.withAuthUrl


{-| Override the credentials JSON encoder to be used when posting to the login
url.

      config
        |> withEncoder
              (\creds ->
                  Encode.object
                      [ ( "credentials"
                        , Encode.dict identity Encode.string creds
                        )
                      ]
              )

-}
withEncoder :
    (Dict String String -> Value)
    -> FormAuth
    -> FormAuth
withEncoder =
    FormAuth.withEncoder


{-| Override the JSON decoder used to obtain the JWT from the login response.

      config
        |> withDecoder
            (Decode.at ["auth", "jwt"] Decode.string)

-}
withDecoder : Decoder String -> FormAuth -> FormAuth
withDecoder =
    FormAuth.withDecoder

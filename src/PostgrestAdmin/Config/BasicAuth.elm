module PostgrestAdmin.Config.BasicAuth exposing
    ( BasicAuth
    , init
    , withAuthUrl
    , withEncoder
    , withDecoder
    )

{-| Program configuration


# Config

@docs BasicAuth
@docs init


# Host

@docs withAuthUrl


# Encode/Decode

@docs withEncoder
@docs withDecoder

-}

import Dict exposing (Dict)
import Internal.BasicAuth as BasicAuth
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


{-| Initialize basic authentication configuration.
-}
type alias BasicAuth =
    Decoder BasicAuth.BasicAuth


{-| Initialize basic authentication configuration.
-}
init : BasicAuth
init =
    BasicAuth.config


{-| Set authentication request login url. Credentials are to be exchanged for a
JWT via a post request.

      init |> withAuthUrl "http://localhost:3000/rpc/login"

Alternatively the host can be specified using flags, configuring using
`withAuthUrl` function takes precedence.

      Elm.Main.init({
          flags: { authUrl: "http://localhost:3000/rpc/login" }
      })

-}
withAuthUrl : String -> BasicAuth -> BasicAuth
withAuthUrl =
    BasicAuth.withAuthUrl


{-| Override the credentials JSON encoder to be used when posting to the login
url.

      init
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
    -> BasicAuth
    -> BasicAuth
withEncoder =
    BasicAuth.withEncoder


{-| Override the JSON decoder used to obtain the JWT from the login response.

      init
        |> withDecoder
            (Decode.at ["auth", "jwt"] Decode.string)

-}
withDecoder : Decoder String -> BasicAuth -> BasicAuth
withDecoder =
    BasicAuth.withDecoder

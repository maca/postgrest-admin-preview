module PostgrestAdmin.Config.BasicAuth exposing
    ( BasicAuth
    , config
    , withAuthUrl
    , withEncoder
    , withDecoder
    )

{-| Program configuration


# Config

@docs BasicAuth
@docs config


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


{-| Basic authentication configuration.
-}
type alias BasicAuth =
    Decoder BasicAuth.BasicAuth


{-| Create a authentication configuration.
-}
config : BasicAuth
config =
    BasicAuth.config


{-| Set authentication request login url. Credentials are to be exchanged for a
JWT via a post request.

      config
          |> withAuthUrl "http://localhost:3000/rpc/login"

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
    -> BasicAuth
    -> BasicAuth
withEncoder =
    BasicAuth.withEncoder


{-| Override the JSON decoder used to obtain the JWT from the login response.

      config
        |> withDecoder
            (Decode.at ["auth", "jwt"] Decode.string)

-}
withDecoder : Decoder String -> BasicAuth -> BasicAuth
withDecoder =
    BasicAuth.withDecoder

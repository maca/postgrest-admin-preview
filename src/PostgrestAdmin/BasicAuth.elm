module PostgrestAdmin.BasicAuth exposing
    ( BasicAuth
    , withAuthUrl
    , withEncoder
    , withDecoder
    , config
    )

{-|

@docs BasicAuth

@docs withAuthUrl


# Request codec

@docs withEncoder
@docs withDecoder

-}

import Dict exposing (Dict)
import Internal.BasicAuth as BasicAuth
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


{-| BasicAuth configurations
-}
type alias BasicAuth =
    Decoder BasicAuth.BasicAuth


{-| initialialize BasicAuth configurations
-}
config : BasicAuth
config =
    BasicAuth.config


{-| Provide a url to perform credentials authentication

    with

-}
withAuthUrl : String -> BasicAuth -> BasicAuth
withAuthUrl =
    BasicAuth.withAuthUrl


withEncoder : (Dict String String -> Value) -> BasicAuth -> BasicAuth
withEncoder =
    BasicAuth.withEncoder


withDecoder : Decoder String -> BasicAuth -> BasicAuth
withDecoder =
    BasicAuth.withDecoder

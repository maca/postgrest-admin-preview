module PostgrestAdmin.BasicAuth exposing
    ( BasicAuth
    , config
    , withAuthUrl
    , withDecoder
    , withEncoder
    )

import Dict exposing (Dict)
import Internal.BasicAuth as BasicAuth
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


type alias BasicAuth =
    Decoder BasicAuth.BasicAuth


config : BasicAuth
config =
    BasicAuth.config


withAuthUrl : String -> BasicAuth -> BasicAuth
withAuthUrl =
    BasicAuth.withAuthUrl


withEncoder : (Dict String String -> Value) -> BasicAuth -> BasicAuth
withEncoder =
    BasicAuth.withEncoder


withDecoder : Decoder String -> BasicAuth -> BasicAuth
withDecoder =
    BasicAuth.withDecoder

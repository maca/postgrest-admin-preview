module PostgrestAdmin.ConfigUtils exposing (optionalFlag)

import Json.Decode as Decode exposing (Decoder)


optionalFlag : String -> (String -> a -> Decoder a) -> (Decoder a -> Decoder a)
optionalFlag flagName updateFunc =
    Decode.andThen
        (\a ->
            Decode.map (Maybe.withDefault a)
                (Decode.maybe
                    (Decode.field flagName Decode.string
                        |> Decode.andThen (\str -> updateFunc str a)
                    )
                )
        )

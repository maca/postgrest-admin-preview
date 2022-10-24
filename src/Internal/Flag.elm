module Internal.Flag exposing
    ( custom
    , linksList
    , string
    , stringDict
    , stringList
    , stringListDict
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


string : String -> (String -> c -> Decoder c) -> Decoder c -> Decoder c
string =
    custom Decode.string


stringDict :
    String
    -> (Dict String String -> c -> Decoder c)
    -> Decoder c
    -> Decoder c
stringDict =
    custom (Decode.dict Decode.string)


stringListDict :
    String
    -> (Dict String (List String) -> c -> Decoder c)
    -> Decoder c
    -> Decoder c
stringListDict =
    custom (Decode.dict (Decode.list Decode.string))


stringList :
    String
    -> (List String -> c -> Decoder c)
    -> Decoder c
    -> Decoder c
stringList =
    custom (Decode.list Decode.string)


linksList :
    String
    -> (List ( String, String ) -> c -> Decoder c)
    -> Decoder c
    -> Decoder c
linksList =
    custom
        (Decode.list
            (Decode.map2 Tuple.pair
                (Decode.field "text" Decode.string)
                (Decode.field "url" Decode.string)
            )
        )


custom : Decoder a -> String -> (a -> c -> Decoder c) -> Decoder c -> Decoder c
custom decoder flagName updateFunc =
    Decode.andThen
        (\a ->
            Decode.map (Maybe.withDefault a)
                (Decode.maybe
                    (Decode.field flagName decoder
                        |> Decode.andThen (\str -> updateFunc str a)
                    )
                )
        )

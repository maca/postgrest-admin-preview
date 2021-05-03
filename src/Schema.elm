module Schema exposing (Field, Schema, decoder)

import Basics.Extra exposing (uncurry)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


type alias Schema =
    List Definition


type Definition
    = Definition String (List Field)


type Value
    = PFloat (Maybe Float)
    | PInt (Maybe Int)
    | PString (Maybe String)
    | PBool (Maybe Bool)
    | PArray (Maybe (List Value))
    | PObject (Maybe (Dict String Value))
    | BadValue String


type Field
    = Field String Value Bool


decoder : Decoder Schema
decoder =
    Decode.map (List.map <| uncurry Definition)
        (Decode.field "definitions" (Decode.keyValuePairs fieldsDecoder))


fieldsDecoder : Decoder (List Field)
fieldsDecoder =
    Decode.field "required" (Decode.list Decode.string)
        |> Decode.andThen propertiesDecoder


propertiesDecoder : List String -> Decoder (List Field)
propertiesDecoder required =
    let
        mapField ( name, value ) =
            Field name value (List.member name required)
    in
    Decode.map (List.map mapField)
        (Decode.field "properties" (Decode.keyValuePairs valueDecoder))


valueDecoder : Decoder Value
valueDecoder =
    let
        map cons dec =
            Decode.map cons (Decode.maybe <| Decode.field "default" dec)
    in
    Decode.map2 Tuple.pair
        (Decode.field "type" Decode.string)
        (Decode.field "format" Decode.string)
        |> Decode.andThen
            (\data ->
                case data of
                    ( "number", _ ) ->
                        map PFloat Decode.float

                    ( "integer", _ ) ->
                        map PInt Decode.int

                    ( "boolean", _ ) ->
                        map PBool Decode.bool

                    ( "string", _ ) ->
                        map PString Decode.string

                    ( "array", _ ) ->
                        map PArray <| Decode.list valueDecoder

                    ( "object", _ ) ->
                        map PObject <| Decode.dict valueDecoder

                    ( k, v ) ->
                        Decode.succeed <|
                            BadValue <|
                                "unknown type ("
                                    ++ k
                                    ++ ", "
                                    ++ v
                                    ++ ")"
            )

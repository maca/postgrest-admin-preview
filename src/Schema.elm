module Schema exposing (Field, Schema, decoder)

import Basics.Extra exposing (uncurry)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


type alias Schema =
    List Definition


type Definition
    = Definition String (List Field)


type Value
    = Value


type alias Field =
    { fieldType : String
    , default : Maybe Value
    , required : Bool
    , name : String
    }


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
        partialFieldDecoder =
            Decode.map2 Field
                (Decode.field "format" typeDecoder)
                (Decode.maybe <| Decode.field "default" defaultDecoder)

        mapField ( name, const ) =
            const (List.member name required) name
    in
    Decode.map (List.map mapField)
        (Decode.field "properties" (Decode.keyValuePairs partialFieldDecoder))


typeDecoder : Decoder String
typeDecoder =
    Decode.string


defaultDecoder : Decoder Value
defaultDecoder =
    Decode.succeed Value

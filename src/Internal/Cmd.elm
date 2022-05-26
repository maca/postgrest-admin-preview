module Internal.Cmd exposing (Cmd(..), map, none, wrap)

import Internal.Client exposing (Client)
import Json.Decode exposing (Value)
import Platform.Cmd as Platform
import Task exposing (Task)
import Utils.Task exposing (Error(..))


type Cmd msg
    = ChildCmd (Platform.Cmd msg)
    | Fetch (Result Error Value -> msg) (Task Never Client)


wrap : Platform.Cmd msg -> Cmd msg
wrap =
    ChildCmd


none : Cmd msg
none =
    wrap Cmd.none


map : (a -> b) -> Cmd a -> Cmd b
map fn cmd =
    case cmd of
        ChildCmd a ->
            wrap (Cmd.map fn a)

        Fetch result task ->
            Fetch (fn << result) task

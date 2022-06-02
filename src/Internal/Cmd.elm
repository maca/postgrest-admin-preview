module Internal.Cmd exposing (Cmd(..), batch, map, none, wrap)

import Internal.Notification as Notification
import Json.Decode exposing (Value)
import Platform.Cmd as Platform
import Task exposing (Task)
import Utils.Task exposing (Error(..))


type Cmd msg
    = ChildCmd (Platform.Cmd msg)
    | Batch (List (Cmd msg))
    | ChangeNotification (Platform.Cmd Notification.Msg)
    | Fetch (Result Error Value -> msg) (Task Error Value)


wrap : Platform.Cmd msg -> Cmd msg
wrap =
    ChildCmd


none : Cmd msg
none =
    wrap Cmd.none


batch : List (Cmd msg) -> Cmd msg
batch =
    Batch


map : (a -> b) -> Cmd a -> Cmd b
map fn cmd =
    case cmd of
        ChildCmd a ->
            wrap (Cmd.map fn a)

        Batch cmds ->
            Batch (List.map (map fn) cmds)

        ChangeNotification a ->
            ChangeNotification a

        Fetch result task ->
            Fetch (fn << result) task

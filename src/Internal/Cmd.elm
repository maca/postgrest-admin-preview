module Internal.Cmd exposing (Cmd(..), batch, none, wrap)

import Internal.Notification as Notification
import Platform.Cmd as Platform


type Cmd msg
    = ChildCmd (Platform.Cmd msg)
    | Batch (List (Cmd msg))
    | ChangeNotification (Platform.Cmd Notification.Msg)


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

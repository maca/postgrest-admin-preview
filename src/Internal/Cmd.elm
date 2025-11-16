module Internal.Cmd exposing (Cmd(..), batch, confirm, dismiss, error, none, wrap)

import Platform.Cmd as Platform


type Cmd msg
    = ChildCmd (Platform.Cmd msg)
    | Batch (List (Cmd msg))
    | NotificationError String
    | NotificationConfirm String
    | NotificationDismiss


wrap : Platform.Cmd msg -> Cmd msg
wrap =
    ChildCmd


none : Cmd msg
none =
    wrap Cmd.none


batch : List (Cmd msg) -> Cmd msg
batch =
    Batch


{-| Display an error.
-}
error : String -> Cmd msg
error message =
    NotificationError message


{-| Display a confirmation notification.
-}
confirm : String -> Cmd msg
confirm message =
    NotificationConfirm message


{-| Dismiss notification.
-}
dismiss : Cmd msg
dismiss =
    NotificationDismiss

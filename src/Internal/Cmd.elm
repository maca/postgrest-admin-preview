module Internal.Cmd exposing (AppCmd(..), batch, confirm, dismiss, error, none, wrap)


type AppCmd msg
    = ChildCmd (Cmd msg)
    | Batch (List (AppCmd msg))
    | NotificationError String
    | NotificationConfirm String
    | NotificationDismiss


wrap : Cmd msg -> AppCmd msg
wrap =
    ChildCmd


none : AppCmd msg
none =
    wrap Cmd.none


batch : List (AppCmd msg) -> AppCmd msg
batch =
    Batch


{-| Display an error.
-}
error : String -> AppCmd msg
error message =
    NotificationError message


{-| Display a confirmation notification.
-}
confirm : String -> AppCmd msg
confirm message =
    NotificationConfirm message


{-| Dismiss notification.
-}
dismiss : AppCmd msg
dismiss =
    NotificationDismiss

module Internal.Cmd exposing (AppCmd(..), batch, clientError, confirm, dismiss, error, none, wrap)

import PostgRestAdmin.Client exposing (Error)


type AppCmd msg
    = ChildCmd (Cmd msg)
    | Batch (List (AppCmd msg))
    | ClientError Error
    | Error String
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
clientError : Error -> AppCmd msg
clientError =
    ClientError


{-| Display an error.
-}
error : String -> AppCmd msg
error =
    Error


{-| Display a confirmation notification.
-}
confirm : String -> AppCmd msg
confirm =
    NotificationConfirm


{-| Dismiss notification.
-}
dismiss : AppCmd msg
dismiss =
    NotificationDismiss

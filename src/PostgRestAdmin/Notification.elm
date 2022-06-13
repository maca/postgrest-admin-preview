module PostgRestAdmin.Notification exposing
    ( confirm
    , error
    , dismiss
    )

{-| Display notifications and errors.

Note that this functions produce a [PostgRestAdmin.Cmd](PostgRestAdmin.Cmd) and
not a vanilla Elm Cmd.

@docs confirm
@docs error
@docs dismiss

-}

import Internal.Cmd as Internal
import Internal.Notification as Notification
import PostgRestAdmin.Cmd as AppCmd
import Task exposing (Task)


{-| Display an error.
-}
error : String -> AppCmd.Cmd msg
error =
    Notification.error >> performTask


{-| Display an confirmation notification.
-}
confirm : String -> AppCmd.Cmd msg
confirm =
    Notification.confirm >> performTask


{-| Dismiss notification.
-}
dismiss : AppCmd.Cmd msg
dismiss =
    performTask Notification.dismiss


performTask : Task Never Notification.Msg -> AppCmd.Cmd msg
performTask =
    Task.perform identity >> Internal.ChangeNotification

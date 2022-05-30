module PostgRestAdmin.Notification exposing
    ( Notification
    , confirm
    , dismiss
    , error
    , none
    )

import Internal.Cmd as Internal
import Internal.Notification as Notification
import PostgRestAdmin.Cmd as AppCmd
import Task exposing (Task)


type alias Notification =
    Notification.Notification


none : Notification
none =
    Notification.none


error : String -> AppCmd.Cmd msg
error =
    Notification.error >> performTask


confirm : String -> AppCmd.Cmd msg
confirm =
    Notification.confirm >> performTask


dismiss : AppCmd.Cmd msg
dismiss =
    performTask Notification.dismiss


performTask : Task Never Notification.Msg -> AppCmd.Cmd msg
performTask =
    Task.perform identity >> Internal.ChangeNotification

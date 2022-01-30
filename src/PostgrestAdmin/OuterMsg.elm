module PostgrestAdmin.OuterMsg exposing (OuterMsg(..))

import Notification
import Utils.Task exposing (Error)


type OuterMsg
    = RequestFailed Error
    | LoginSuccess String
    | NotificationChanged Notification.Msg
    | Pass

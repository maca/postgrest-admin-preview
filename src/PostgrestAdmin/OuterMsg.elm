module PostgrestAdmin.OuterMsg exposing (OuterMsg(..))

import Utils.Task exposing (Error)


type OuterMsg
    = RequestFailed Error
    | Pass

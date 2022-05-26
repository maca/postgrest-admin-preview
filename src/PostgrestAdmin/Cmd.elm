module PostgrestAdmin.Cmd exposing (Cmd, map, none, wrap)

import Internal.Cmd as Internal exposing (Cmd)
import Platform.Cmd as Platform


type alias Cmd msg =
    Internal.Cmd msg


wrap : Platform.Cmd msg -> Cmd msg
wrap =
    Internal.wrap


none : Cmd msg
none =
    wrap Cmd.none


map : (a -> b) -> Cmd a -> Cmd b
map =
    Internal.map

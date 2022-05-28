module PostgRestAdmin.Cmd exposing
    ( Cmd
    , wrap
    , none
    , map
    )

{-| Program configuration

@docs Cmd
@docs wrap
@docs none
@docs map

-}

import Internal.Cmd as Internal exposing (Cmd)
import Platform.Cmd as Platform


{-| Wrapper around the vanilla Elm
[Cmd](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#Cmd)
for internal communication.

When mounting an application via [Config.withMountPoint](PostgRestAdmin.Config#withMountPoint),
your app should use this type instead of
[Platform.Cmd](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#Cmd),
then it's just matter of using [wrap](#wrap) to wrap the Cmd.

[PostgRestAdmin.Client](PostgRestAdmin.Client) speaks this language.

-}
type alias Cmd msg =
    Internal.Cmd msg


{-| Wraps the [Platform.Cmd](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#Cmd).

      import Time exposing (Posix)

      getNewTime : (Posix -> Msg) -> PostgRestAdmin.Cmd Msg
      getNewTime tagger =
        Task.perform tagger Time.now
            |> PostgRestAdmin.Cmd.wrap

-}
wrap : Platform.Cmd msg -> Cmd msg
wrap =
    Internal.wrap


{-| Do nothing.

    wrap Platform.Cmd.none == none

-}
none : Cmd msg
none =
    wrap Cmd.none


{-| Transform the messages produced by a command. Similar to
[Platform.Cmd](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#map).
-}
map : (a -> b) -> Cmd a -> Cmd b
map =
    Internal.map

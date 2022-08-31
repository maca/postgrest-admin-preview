module PostgRestAdmin.Cmd exposing
    ( Cmd
    , wrap
    , none
    , batch
    , map
    )

{-| Program configuration

@docs Cmd
@docs wrap
@docs none
@docs batch
@docs map

-}

import Internal.Cmd as Internal exposing (Cmd)
import Platform.Cmd as Platform


{-| Wrapper around the vanilla Elm
[Cmd](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#Cmd)
for internal communication.

When mounting an application via [Config.routes](PostgRestAdmin.Config#routes),
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


{-| Pass a bunch of commands to the runtime, execution order is not guaranteed.

Equivalent to
[Platform.Cmd.batch](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#batch).

-}
batch : List (Cmd msg) -> Cmd msg
batch =
    Internal.batch


{-| Transform the messages produced by a command.

Equivalent to
[Platform.Cmd.map](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#map).

-}
map : (a -> b) -> Cmd a -> Cmd b
map =
    Internal.map

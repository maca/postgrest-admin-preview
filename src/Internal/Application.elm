module Internal.Application exposing
    ( Application(..)
    , Params
    , none
    , subscriptions
    , update
    )

import Browser.Navigation as Nav
import Html exposing (Html)
import Internal.Cmd as AppCmd
import Json.Decode as Decode
import PostgRestAdmin.Client exposing (Client)


type alias Params flags model msg =
    { init : flags -> Client -> Nav.Key -> ( model, AppCmd.Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, AppCmd.Cmd msg )
    , subscriptions : model -> Sub msg
    , onLogin : Client -> msg
    }


type Application flags model msg
    = Application (Params flags model msg) model
    | DecodeFailed Decode.Error
    | None


none : Application f model msg
none =
    None


update : msg -> Application f m msg -> ( Application f m msg, AppCmd.Cmd msg )
update msg application =
    case application of
        Application params model ->
            params.update msg model |> Tuple.mapFirst (Application params)

        _ ->
            ( application, AppCmd.none )


subscriptions : Application f m msg -> Sub msg
subscriptions application =
    case application of
        Application params model ->
            params.subscriptions model

        _ ->
            Sub.none

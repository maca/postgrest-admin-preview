module Internal.Application exposing
    ( Application(..)
    , Params
    , none
    , subscriptions
    , update
    )

import Html exposing (Html)
import Internal.Cmd as AppCmd
import PostgRestAdmin.Client exposing (Client)


type alias Params model msg =
    { init : Client -> ( model, AppCmd.Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, AppCmd.Cmd msg )
    , subscriptions : model -> Sub msg
    , onLogin : Client -> msg
    }


type Application model msg
    = Application (Params model msg) model
    | None


none : Application model msg
none =
    None


update : msg -> Application m msg -> ( Application m msg, AppCmd.Cmd msg )
update msg application =
    case application of
        Application params model ->
            params.update msg model |> Tuple.mapFirst (Application params)

        None ->
            ( application, AppCmd.none )


subscriptions : Application m msg -> Sub msg
subscriptions application =
    case application of
        Application params model ->
            params.subscriptions model

        None ->
            Sub.none

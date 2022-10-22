module PostgRestAdmin.Detail exposing
    ( Detail
    , Msg
    , init
    , update
    , view
    )

{-| Detail

@docs Detail
@docs Msg


# Init

@docs init
@docs update
@docs view

-}

import Browser.Navigation as Nav
import Html exposing (Html)
import Internal.PageDetail as PageDetail exposing (PageDetail)
import Internal.Schema exposing (Constraint(..), Table)
import PostgRestAdmin.Client exposing (Client)
import PostgRestAdmin.Cmd as AppCmd
import PostgRestAdmin.MountPath exposing (MountPath)
import PostgRestAdmin.Record exposing (Record)


{-| -}
type Msg
    = Msg PageDetail.Msg


{-| -}
type Detail
    = Detail PageDetail


{-| -}
init :
    { client : Client
    , mountPath : MountPath
    , table : Table
    , id : String
    , detailActions : List ( String, Record -> String -> String )
    }
    -> Nav.Key
    -> ( Detail, AppCmd.Cmd Msg )
init params navKey =
    PageDetail.init params navKey
        |> Tuple.mapFirst Detail
        |> Tuple.mapSecond (AppCmd.map Msg)


{-| -}
update : Msg -> Detail -> ( Detail, AppCmd.Cmd Msg )
update (Msg msg) (Detail detail) =
    PageDetail.update msg detail
        |> Tuple.mapFirst Detail
        |> Tuple.mapSecond (AppCmd.map Msg)


{-| -}
view : Detail -> Html Msg
view (Detail detail) =
    Html.map Msg (PageDetail.view detail)

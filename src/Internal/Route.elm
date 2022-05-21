module Internal.Route exposing
    ( Application
    , InitParams
    , MountPoint(..)
    , Route(..)
    )

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (Html)
import Internal.Cmd as AppCmd
import Internal.Msg exposing (Msg)
import Internal.PageDetail exposing (PageDetail)
import Internal.PageForm exposing (PageForm)
import Internal.PageListing exposing (PageListing)
import PostgrestAdmin.Client exposing (Client)
import Url.Parser exposing (Parser)


type alias Application model msg =
    { init : Client -> ( model, AppCmd.Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, AppCmd.Cmd msg )
    , onLogin : Client -> msg
    }


type alias InitParams m msg =
    { client : Client
    , formFields : Dict String (List String)
    , key : Nav.Key
    , application : Maybe (MountPoint m msg)
    }


type MountPoint m msg
    = MountPoint
        (Application m msg)
        (Parser
            (() -> ( Route m msg, Cmd (Msg msg) ))
            ( Route m msg, Cmd (Msg msg) )
        )


type Route m msg
    = RouteRoot
    | RouteLoadingSchema (InitParams m msg -> ( Route m msg, Cmd (Msg msg) ))
    | RouteListing PageListing
    | RouteDetail PageDetail
    | RouteForm PageForm
    | RouteApplication (Application m msg) m
    | RouteNotFound

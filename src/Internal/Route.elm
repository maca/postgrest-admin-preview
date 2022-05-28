module Internal.Route exposing
    ( InitParams
    , MountPoint
    , Route(..)
    )

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Internal.Application exposing (Params)
import Internal.Msg exposing (Msg)
import Internal.PageDetail exposing (PageDetail)
import Internal.PageForm exposing (PageForm)
import Internal.PageListing exposing (PageListing)
import PostgrestAdmin.Client exposing (Client)
import Url.Parser exposing (Parser)


type alias InitParams m msg =
    { client : Client
    , formFields : Dict String (List String)
    , key : Nav.Key
    , application : Maybe (MountPoint m msg)
    }


type alias MountPoint m msg =
    ( Params m msg
    , Parser
        (msg -> ( Route m msg, Cmd (Msg m msg) ))
        ( Route m msg, Cmd (Msg m msg) )
    )


type Route m msg
    = RouteRoot
    | RouteLoadingSchema (InitParams m msg -> ( Route m msg, Cmd (Msg m msg) ))
    | RouteListing PageListing
    | RouteDetail PageDetail
    | RouteForm PageForm
    | RouteApplication
    | RouteNotFound

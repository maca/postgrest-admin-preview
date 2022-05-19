module PostgrestAdmin.Route exposing
    ( MountPoint
    , Program
    , Route(..)
    )

import Html exposing (Html)
import PageDetail exposing (PageDetail)
import PageForm exposing (PageForm)
import PageListing exposing (PageListing)
import Postgrest.Record exposing (Record)
import Postgrest.Schema exposing (Schema)
import Url.Parser exposing (Parser)


type alias Program model msg =
    { init : Record -> ( model, Cmd msg )
    , view : model -> Html msg
    , update : msg -> model -> ( model, Cmd msg )
    }


type alias Resource =
    { tableName : String
    , id : String
    }


type alias MountPoint m msg =
    { program : Program m msg
    , parser : Parser (String -> String -> Route m msg) (Route m msg)
    }


type Route model msg
    = RouteRoot
    | RouteLoadingSchema (Schema -> Route model msg)
    | RouteLoadingResource Resource (Record -> Route model msg)
    | RouteListing PageListing
    | RouteDetail PageDetail
    | RouteForm PageForm
    | RouteResource (Program model msg) ( model, Cmd msg )
    | RouteNotFound

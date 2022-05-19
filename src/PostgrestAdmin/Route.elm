module PostgrestAdmin.Route exposing (Route(..))

import Html exposing (Html)
import PageDetail exposing (Detail)
import PageForm exposing (Form)
import PageListing exposing (Listing)
import Postgrest.Record exposing (Record)
import Postgrest.Schema as Schema exposing (Schema, Table)


type Route model msg
    = RouteRoot
    | RouteLoadingSchema (Schema -> Route model msg)
    | RouteFormLoading
        { tableName : String
        , id : String
        }
        (Record -> Route model msg)
    | RouteListing Listing
    | RouteDetail Detail
    | RouteForm Form
    | RouteCustom
        { view : model -> Html msg
        , update : msg -> model -> ( model, Cmd msg )
        , init : Record -> ( model, Cmd msg )
        }
        (Maybe model)
    | RouteNotFound

module PostgrestAdmin.Route exposing (Route(..))

import Html exposing (Html)
import PageDetail exposing (PageDetail)
import PageForm exposing (PageForm)
import PageListing exposing (PageListing)
import Postgrest.Record exposing (Record)
import Postgrest.Schema exposing (Schema)


type Route model msg
    = RouteRoot
    | RouteLoadingSchema (Schema -> Route model msg)
    | RouteLoadingResource
        { tableName : String
        , id : String
        }
        (Record -> Route model msg)
    | RouteListing PageListing
    | RouteDetail PageDetail
    | RouteForm PageForm
    | RouteResource
        { view : model -> Html msg
        , update : msg -> model -> ( model, Cmd msg )
        , init : Record -> ( model, Cmd msg )
        }
        (Maybe model)
    | RouteNotFound

module PostgrestAdmin.Route exposing (Route(..))

import Detail exposing (Detail)
import FormPage exposing (Form)
import Html exposing (Html)
import ListingPage exposing (Listing)
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
    | RouteNotFound

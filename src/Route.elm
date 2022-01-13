module Route exposing (Route(..), toForm, toListing)

import Form exposing (Form)
import Listing exposing (Listing)
import Postgrest.Schema.Definition exposing (Definition)


type Route
    = Root
    | LoadingDefinition String (Definition -> Route)
    | Listing Listing
    | FormLoading Form String
    | Form Form
    | NotFound


toListing : Route -> Maybe Listing
toListing route =
    case route of
        Listing listing ->
            Just listing

        _ ->
            Nothing


toForm : Route -> Maybe Form
toForm route =
    case route of
        FormLoading form _ ->
            Just form

        Form form ->
            Just form

        _ ->
            Nothing

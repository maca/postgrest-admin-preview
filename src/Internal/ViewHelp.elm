module Internal.ViewHelp exposing (breadcrumbs)

import Html exposing (Html, a, h1, span, text)
import Html.Attributes exposing (class, classList, href, style)
import String.Extra as String
import Url.Builder as Url


breadcrumbs : String -> List String -> Html msg
breadcrumbs tableName components =
    h1 [ style "font-size" "inherit" ]
        [ span
            [ class "breadcrumbs" ]
            (breadcrumbsHelp tableName [] (List.reverse components))
        ]


breadcrumbsHelp : String -> List (Html msg) -> List String -> List (Html msg)
breadcrumbsHelp tableName acc components =
    case components of
        comp :: rest ->
            breadcrumbsHelp tableName
                (span [ class "divisor" ] [ text " / " ]
                    :: a
                        [ classList [ ( "current-segment", tableName == comp ) ]
                        , href (Url.absolute (List.reverse components) [])
                        ]
                        [ String.humanize comp
                            |> String.toTitleCase
                            |> text
                        ]
                    :: acc
                )
                rest

        [] ->
            acc

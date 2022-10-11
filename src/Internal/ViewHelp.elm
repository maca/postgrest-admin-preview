module Internal.ViewHelp exposing (breadcrumbs)

import Html exposing (Html, a, h1, span, text)
import Html.Attributes exposing (class, classList, href, style)
import String.Extra as String
import Url.Builder as Url


breadcrumbs : String -> List ( String, Maybe String ) -> Html msg
breadcrumbs tableName segments =
    h1 [ style "font-size" "inherit" ]
        [ span
            [ class "breadcrumbs" ]
            (breadcrumbsHelp tableName [] (List.reverse segments))
        ]


breadcrumbsHelp :
    String
    -> List (Html msg)
    -> List ( String, Maybe String )
    -> List (Html msg)
breadcrumbsHelp tableName acc segments =
    case segments of
        ( segment, segmentText ) :: rest ->
            breadcrumbsHelp tableName
                (span [ class "divisor" ] [ text " / " ]
                    :: a
                        [ classList
                            [ ( "current-segment", tableName == segment ) ]
                        , href
                            (Url.absolute
                                (List.map Tuple.first (List.reverse segments))
                                []
                            )
                        ]
                        [ text (String.toTitleCase (String.humanize segment))
                        , case segmentText of
                            Just str ->
                                text (" – " ++ str)

                            Nothing ->
                                text ""
                        ]
                    :: acc
                )
                rest

        [] ->
            acc

module PostgRestAdmin.MountPoint exposing
    ( MountPoint
    , breadcrumbs
    , fromString
    , path
    , segments
    )

import Html exposing (Html, a, h1, span, text)
import Html.Attributes exposing (class, classList, href, style)
import Internal.Http exposing (removeLeadingOrTrailingSlash)
import String.Extra as String
import Url.Builder as Url


{-|


# Mount Point

@docs MountPoint, fromString, toString

-}
type MountPoint
    = MountPoint String
    | Blank


fromString : String -> MountPoint
fromString m =
    String.nonBlank (removeLeadingOrTrailingSlash m)
        |> Maybe.map MountPoint
        |> Maybe.withDefault Blank


segments : MountPoint -> List String
segments mountPoint =
    case mountPoint of
        MountPoint s ->
            String.split "/" s

        Blank ->
            []


path : MountPoint -> String -> String
path mountPoint segment =
    case mountPoint of
        MountPoint m ->
            String.join "/" [ "", m, removeLeadingOrTrailingSlash segment ]

        Blank ->
            segment


breadcrumbs :
    MountPoint
    -> String
    -> List ( String, Maybe String )
    -> Html msg
breadcrumbs mountPoint tableName components =
    h1
        [ style "font-size" "inherit" ]
        [ span
            [ class "breadcrumbs" ]
            (breadcrumbsHelp mountPoint tableName [] (List.reverse components))
        ]


breadcrumbsHelp :
    MountPoint
    -> String
    -> List (Html msg)
    -> List ( String, Maybe String )
    -> List (Html msg)
breadcrumbsHelp mountPoint tableName acc components =
    case components of
        ( segment, segmentText ) :: rest ->
            breadcrumbsHelp mountPoint
                tableName
                (span [ class "divisor" ] [ text " / " ]
                    :: a
                        [ classList
                            [ ( "current-segment", tableName == segment ) ]
                        , href
                            (Url.absolute
                                (List.map Tuple.first (List.reverse components))
                                []
                                |> path mountPoint
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

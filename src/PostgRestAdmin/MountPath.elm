module PostgRestAdmin.MountPath exposing
    ( MountPath, fromString, segments
    , path, breadcrumbs
    )

{-| PostgRestAdmin mount path and url generation.

@docs MountPath, fromString, segments


# Path building

@docs path, breadcrumbs

-}

import Html exposing (Html, a, h1, span, text)
import Html.Attributes exposing (class, classList, href, style)
import Internal.Http exposing (removeLeadingOrTrailingSlash)
import String.Extra as String
import Url.Builder as Url


{-| Represents a path where PostgRestAdmin is mounted.
See [Config.mountPath](PostgRestAdmin.Config#mountPath) to mount it at any
path other than `/`.
-}
type MountPath
    = MountPath String
    | Blank


{-| Build from `String`.

    fromString "admin" == fromString "/admin"

    fromString "" == fromString "/"

-}
fromString : String -> MountPath
fromString m =
    String.nonBlank (removeLeadingOrTrailingSlash m)
        |> Maybe.map MountPath
        |> Maybe.withDefault Blank


{-| Convert to a list of path segments.

    segments (fromString "/my/mount/point") == [ "my", "mount", "point" ]

-}
segments : MountPath -> List String
segments mountPath =
    case mountPath of
        MountPath s ->
            String.split "/" s

        Blank ->
            []


{-| Build a path prepended by the endpoint.

    path (fromString "admin") "some/path" == "/admin/some/path"

-}
path : MountPath -> String -> String
path mountPath segment =
    case mountPath of
        MountPath m ->
            String.join "/" [ "", m, removeLeadingOrTrailingSlash segment ]

        Blank ->
            segment


{-| Html breadcrumbs for a path prepended by the endpoint.

    myHeader : Html msg
    myHeader =
        Html.header
            []
            [ Html.h1 [] [ Html.text "Some Path" ]
            , breadcrumbs (fromString "admin") "some/path"
            ]

-}
breadcrumbs :
    MountPath
    -> String
    -> List ( String, Maybe String )
    -> Html msg
breadcrumbs mountPath tableName components =
    h1
        [ style "font-size" "inherit" ]
        [ span
            [ class "breadcrumbs" ]
            (breadcrumbsHelp mountPath tableName [] (List.reverse components))
        ]


breadcrumbsHelp :
    MountPath
    -> String
    -> List (Html msg)
    -> List ( String, Maybe String )
    -> List (Html msg)
breadcrumbsHelp mountPath tableName acc components =
    case components of
        ( segment, segmentText ) :: rest ->
            breadcrumbsHelp mountPath
                tableName
                (span [ class "divisor" ] [ text " / " ]
                    :: a
                        [ classList
                            [ ( "current-segment", tableName == segment ) ]
                        , href
                            (Url.absolute
                                (List.map Tuple.first (List.reverse components))
                                []
                                |> path mountPath
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

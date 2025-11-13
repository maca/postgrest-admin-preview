module PostgRestAdmin.Views exposing (renderValue)

import Html exposing (Html)
import Html.Attributes as Attrs
import Internal.Schema exposing (Column, ColumnType(..), Value(..))
import Iso8601
import PostgRestAdmin.MountPath as MountPath exposing (MountPath)
import Time.Extra as Time


renderValue : MountPath -> Column -> Value -> Html msg
renderValue mountPath col value =
    case ( value, col.columnType ) of
        ( Float f, _ ) ->
            Html.text (String.fromFloat f)

        ( Int i, _ ) ->
            Html.text (String.fromInt i)

        ( Bool b, _ ) ->
            Html.text
                (if b then
                    "true"

                 else
                    "false"
                )

        ( String s, TimestampCol ) ->
            Html.text
                (Iso8601.toTime s
                    |> Result.map Time.format
                    |> Result.withDefault s
                )

        ( String s, TimestampWithoutTimezomeCol ) ->
            Html.text
                (Iso8601.toTime s
                    |> Result.map Time.format
                    |> Result.withDefault s
                )

        ( String s, DateCol ) ->
            Html.text
                (Iso8601.toTime s
                    |> Result.map Time.toDateString
                    |> Result.withDefault s
                )

        ( String s, TimeCol ) ->
            Html.text
                (Iso8601.toTime s
                    |> Result.map Time.format
                    |> Result.withDefault s
                )

        ( String s, TimeWithoutTimezoneCol ) ->
            Html.text
                (Iso8601.toTime s
                    |> Result.map Time.format
                    |> Result.withDefault s
                )

        ( String s, JsonCol ) ->
            Html.pre [] [ Html.text s ]

        ( String s, TextCol ) ->
            Html.text s

        ( String s, _ ) ->
            Html.text s

        ( Blank, _ ) ->
            Html.text ""

        ( Ref ref, _ ) ->
            Html.a
                [ Attrs.href
                    (MountPath.path mountPath (ref.tableName ++ "/" ++ ref.primaryKey))
                ]
                [ Html.text (ref.label ++ " - " ++ ref.primaryKey) ]

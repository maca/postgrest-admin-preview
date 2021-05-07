module Time.Extra exposing (decoder, format)

import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Time exposing (Month(..), Weekday(..), utc)


decoder : Decoder Time.Posix
decoder =
    Decode.string
        |> Decode.andThen
            (Iso8601.toTime
                >> Result.map Decode.succeed
                >> Result.withDefault (Decode.fail "")
            )


format : Time.Posix -> String
format time =
    toDateString time ++ " " ++ toTimeString time


toDateString : Time.Posix -> String
toDateString time =
    let
        date =
            [ Time.toMonth utc time |> toEnglishMonth
            , Time.toDay utc time |> String.fromInt
            , Time.toYear utc time |> String.fromInt
            ]
                |> String.join " "
    in
    (Time.toWeekday utc time |> toEnglishWeekday) ++ ", " ++ date


toTimeString : Time.Posix -> String
toTimeString time =
    let
        toString =
            String.fromInt >> String.pad 2 '0'
    in
    [ Time.toHour utc time |> toString
    , Time.toMinute utc time |> toString
    , Time.toSecond utc time |> toString
    ]
        |> String.join ":"


toEnglishMonth : Month -> String
toEnglishMonth weekday =
    case weekday of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


toEnglishWeekday : Weekday -> String
toEnglishWeekday weekday =
    case weekday of
        Mon ->
            "Mon"

        Tue ->
            "Tue"

        Wed ->
            "Wed"

        Thu ->
            "Thu"

        Fri ->
            "Fri"

        Sat ->
            "Sat"

        Sun ->
            "Sun"

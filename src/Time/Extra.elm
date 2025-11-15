module Time.Extra exposing (format, hours24, parse, toDateString)

import Basics.Extra exposing (flip)
import Iso8601
import Time exposing (Month(..), Weekday(..), utc)


format : Time.Posix -> String
format time =
    toDateString time ++ " " ++ " at " ++ toTimeString time


parse : String -> Maybe Time.Posix
parse =
    String.slice 0 16
        >> flip (++) ":00"
        >> Iso8601.toTime
        >> Result.toMaybe


hours24 : Int
hours24 =
    86400000


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

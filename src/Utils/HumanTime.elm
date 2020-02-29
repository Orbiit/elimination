module Utils.HumanTime exposing (..)

import Time


weekdayNameEnglish : Time.Weekday -> String
weekdayNameEnglish weekday =
    case weekday of
        Time.Mon ->
            "Monday"

        Time.Tue ->
            "Tuesday"

        Time.Wed ->
            "Wednesday"

        Time.Thu ->
            "Thursday"

        Time.Fri ->
            "Friday"

        Time.Sat ->
            "Saturday"

        Time.Sun ->
            "Sunday"


monthNameEnglish : Time.Month -> String
monthNameEnglish month =
    case month of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"


monthName : Time.Month -> String
monthName month =
    case month of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"


display : Time.Zone -> Int -> String
display zone ms =
    let
        time =
            Time.millisToPosix ms
    in
    String.fromInt (Time.toYear zone time)
        ++ "-"
        ++ monthName (Time.toMonth zone time)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toDay zone time))
        ++ " ("
        ++ weekdayNameEnglish (Time.toWeekday zone time)
        ++ ") at "
        ++ String.padLeft 2 '0' (String.fromInt (Time.toHour zone time))
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute zone time))

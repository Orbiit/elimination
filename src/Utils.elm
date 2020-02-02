module Utils exposing (char, Char(..), extLink)

import Html exposing (Html, a, text)
import Html.Attributes exposing (href, rel, target, class)

type Char
  = Middot

char : Char -> String
char specialChar =
  case specialChar of
    Middot ->
      "\u{00b7}"

extLink : String -> String -> String -> Html msg
extLink linkText url className =
  a [ href url, rel "noopener noreferrer", target "_blank", class className ]
    [ text linkText ]

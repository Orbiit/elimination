module Utils exposing (..)

import Html exposing (..)
import Html.Attributes as A

filter : List (Maybe a) -> List a
filter list =
  List.filterMap identity list

type Char
  = Middot
  | MDash

char : Char -> String
char specialChar =
  case specialChar of
    Middot ->
      "\u{00b7}"
    MDash ->
      "\u{2014}"

extLink : String -> String -> String -> Html msg
extLink linkText url className =
  a [ A.href url, A.rel "noopener noreferrer", A.target "_blank", A.class className ]
    [ text linkText ]

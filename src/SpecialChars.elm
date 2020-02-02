module SpecialChars exposing (char, Char(..))

type Char
  = Middot

char : Char -> String
char specialChar =
  case specialChar of
    Middot ->
      "\u{00b7}"

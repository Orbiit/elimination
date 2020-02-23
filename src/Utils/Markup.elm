module Utils.Markup exposing (markup)

import Html exposing (Html, a, text)
import Html.Attributes as A
import Parser exposing (..)
import Char
import Set

type alias Htmls msg = List (Html msg)

username : Parser String
username =
  variable
    { start = \c -> c == '@'
    , inner = \c -> Char.isLower c || Char.isDigit c || c == '-' || c == '_'
    , reserved = Set.empty
    }


gameID : Parser String
gameID =
  variable
    { start = \c -> c == '!'
    , inner = \c -> Char.isLower c || Char.isDigit c || c == '-' || c == '_'
    , reserved = Set.empty
    }

makeInternalLink : Parser (Html msg)
makeInternalLink =
  succeed (\name -> a [ A.class "link", A.href ("?" ++ name) ] [ text name ])
    |= oneOf
      [ username
      , gameID
      ]

normalText : Parser (Html msg)
normalText =
  chompWhile (\c -> c /= '@' && c /= '!' && c /= '\\')
    |> getChompedString
    |> map (\chunk -> text chunk)

markupLoop : Htmls msg -> Parser (Step (Htmls msg) (Htmls msg))
markupLoop reverseHtmls =
  oneOf
    [ succeed (\html -> Loop (html :: reverseHtmls))
      |= oneOf
        [ makeInternalLink
        , normalText
        ]
    , end
      |> map (\_ -> Done (List.reverse reverseHtmls))
    ]



markupParser : Parser (Htmls msg)
markupParser =
  loop [] markupLoop

markup : String -> Htmls msg
markup input =
  case run markupParser input of
    Ok html ->
      html
    Err _ ->
      [ text input ]

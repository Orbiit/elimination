module Utils.MarkupSimple exposing (markup)

import Html exposing (Html, a, text)
import Html.Attributes as A
import Parser exposing (..)
import Char
import Set

type Item
  = Text String
  | Link String

markupLoop : List Item -> Parser (Step (List (Html msg)) (List Item))
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

markupParser : Parser (List (Html msg))
markupParser =
  loop [] markupLoop

markup : String -> List (Html msg)
markup input =
  case run markupParser input of
    Ok html ->
      html
    Err _ ->
      [ text input ]

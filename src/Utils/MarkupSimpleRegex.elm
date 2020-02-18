module Utils.MarkupSimpleRegex exposing (markup)

import Html exposing (Html, a, text)
import Html.Attributes as A
import Regex

import Utils

urlRegex = Utils.makeRegex "\\bhttps?://([-A-Za-z0-9+@#/%?=~_|!:,.;]|&)*[-A-Za-z0-9#/=_]"

type alias FoldState msg =
  { lastIndex : Int
  , htmls : List (Html msg)
  }

fold : String -> Regex.Match -> FoldState msg -> FoldState msg
fold totalString match { lastIndex, htmls } =
  { lastIndex = match.index + String.length match.match
  , htmls =
    List.concat
      [ htmls
      , if match.index == lastIndex then
          []
        else
          [ text (String.slice lastIndex match.index totalString) ]
      , [ a [ A.class "link", A.href match.match ] [ text match.match ] ]
      ]
  }

markup : String -> List (Html msg)
markup input =
  let
    finalState = (Regex.find urlRegex input)
      |> List.foldl (fold input) { lastIndex = 0, htmls = [] }
  in
    finalState.htmls ++
    if finalState.lastIndex == String.length input then
      []
    else
      [ text (String.dropLeft finalState.lastIndex input) ]

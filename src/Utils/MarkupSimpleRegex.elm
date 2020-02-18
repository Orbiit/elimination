module Utils.MarkupSimpleRegex exposing (markup)

import Html exposing (Html, a, text, em, strong)
import Html.Attributes as A
import Regex

import Utils

urlRegex = Utils.makeRegex "\\bhttps?://[-A-Za-z0-9+@#/%?=~_|!:,.;&]*[-A-Za-z0-9#/=_]|@[a-z0-9_-]{3,20}\\b|![0-9a-f]{5}|\\*{1,2}|\\\\."

type alias FoldState msg =
  { lastIndex : Int
  , htmls : List (Html msg)
  , bold : Bool
  , italics : Bool
  }

format : FoldState msg -> List (Html msg) -> List (Html msg)
format { bold, italics } content =
  let
    inner =
      if italics then
        [ em [] content ]
      else
        content
  in
  if bold then
    [ strong [] inner ]
  else
    inner

fold : String -> Regex.Match -> FoldState msg -> FoldState msg
fold totalString match state =
  let
    lastIndex = match.index + String.length match.match
    htmls =
      List.concat
        [ state.htmls
        , if match.index == state.lastIndex then
            []
          else
            format state [ text (String.slice state.lastIndex match.index totalString) ]
        , let
            firstChar = String.left 1 match.match
          in
            if firstChar == "@" || firstChar == "!" then
              format state [ a [ A.class "link", A.href ("?" ++ match.match) ] [ text match.match ] ]
            else if firstChar == "*" then
              []
            else if firstChar == "\\" then
              format state [ text (String.dropLeft 1 match.match) ]
            else
              format state [ Utils.extLink match.match match.match "link" ]
        ]
    newState = { state | lastIndex = lastIndex, htmls = htmls }
  in
    if match.match == "*" then
      { newState | italics = not state.italics }
    else if match.match == "**" then
      { newState | bold = not state.bold }
    else
      newState

markup : String -> List (Html msg)
markup input =
  let
    finalState = (Regex.find urlRegex input)
      |> List.foldl (fold input)
        { lastIndex = 0
        , htmls = []
        , bold = False
        , italics = False
        }
  in
    finalState.htmls ++
    if finalState.lastIndex == String.length input then
      []
    else
      format finalState [ text (String.dropLeft finalState.lastIndex input) ]

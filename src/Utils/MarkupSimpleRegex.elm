module Utils.MarkupSimpleRegex exposing (markup)

import Html exposing (Html, a, text, em, strong)
import Html.Attributes as A
import Regex
import Dict exposing (Dict)

import Utils
import Api

urlRegex = Utils.makeRegex "\\bhttps?://[-A-Za-z0-9+@#/%?=~_|!:,.;&]*[-A-Za-z0-9#/=_]|@[a-z0-9_-]{3,20}(?=[^\\w-])|![0-9a-f]{5}|\\*{1,2}|\\\\."

type alias FoldState msg =
  { lastIndex : Int
  , htmls : List (Html msg)
  , bold : Bool
  , italics : Bool
  , gameIDs : List Api.GameID
  , gameNames : Maybe (Dict Api.GameID String)
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
            if firstChar == "@" then
              format state [ a [ A.class "link", A.href ("?" ++ match.match) ] [ text match.match ] ]
            else if firstChar == "!" then
              format state
                [ a [ A.class "link profile-game-name", A.href ("?" ++ match.match) ]
                  [ text <|
                    case state.gameNames of
                      Just names ->
                        let
                          maybeName = Dict.get (String.dropLeft 1 match.match) names
                        in
                        case maybeName of
                          Just name -> name
                          Nothing -> match.match
                      Nothing ->
                        match.match
                  ]
                ]
            else if firstChar == "*" then
              []
            else if firstChar == "\\" then
              format state [ text (String.dropLeft 1 match.match) ]
            else
              format state [ Utils.extLink match.match match.match "link" ]
        ]
    newState =
      { state
      | lastIndex = lastIndex
      , htmls = htmls
      , gameIDs =
        case (String.left 1 match.match, state.gameNames) of
          ("!", Nothing) ->
            String.dropLeft 1 match.match :: state.gameIDs
          _ ->
            state.gameIDs
      }
  in
    if match.match == "*" then
      { newState | italics = not state.italics }
    else if match.match == "**" then
      { newState | bold = not state.bold }
    else
      newState

type alias MarkupOutput msg =
  { html : List (Html msg)
  , gameIDs : List Api.GameID
  }

markup : Maybe (Dict Api.GameID String) -> String -> MarkupOutput msg
markup gameNames input =
  let
    finalState = (Regex.find urlRegex input)
      |> List.foldl (fold input)
        { lastIndex = 0
        , htmls = []
        , bold = False
        , italics = False
        , gameIDs = []
        , gameNames = gameNames
        }
  in
  { html =
    finalState.htmls ++
      if finalState.lastIndex == String.length input then
        []
      else
        format finalState [ text (String.dropLeft finalState.lastIndex input) ]
  , gameIDs = finalState.gameIDs
  }

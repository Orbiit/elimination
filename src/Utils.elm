module Utils exposing (..)

import Html exposing (..)
import Html.Attributes as A
import Regex
import Browser.Dom as Dom
import Task

makeRegex : String -> Regex.Regex
makeRegex string =
  case Regex.fromString string of
    Just regex ->
      regex
    Nothing ->
      Regex.never

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

-- Based on
-- https://github.com/elm/package.elm-lang.org/blob/a8b9f08c66ddc2a5f784bedbc943f48f6efa9be3/src/frontend/Page/Docs.elm#L115
scrollIfNeeded : msg -> Maybe String -> Cmd msg
scrollIfNeeded msg fragment =
  case fragment of
    Just "" ->
      Task.attempt (\_ -> msg) (Dom.setViewport 0 0)
    Just id ->
      Task.attempt (\_ -> msg) (
        Dom.getElement id
          |> Task.andThen (\info -> Dom.setViewport 0 info.element.y)
      )
    Nothing ->
      Cmd.none

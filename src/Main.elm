module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Url exposing (Url)

type Model
  = Yes

init : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init () url navKey =
  ( Yes, Cmd.none )

view : Model -> Document Msg
view model =
  { title = "Epic test page"
  , body =
    [ div [ class "" ]
      [ text "Okay cool" ]
    ]
  }

type Msg
  = ChangedUrl Url
  | ClickedLink Browser.UrlRequest

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model) of
    ( _, _ ) ->
      -- Disregard messages that arrived for the wrong page.
      ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlChange = ChangedUrl
    , onUrlRequest = ClickedLink
    }

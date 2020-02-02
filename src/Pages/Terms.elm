module Pages.Terms exposing (termsPage)

import Html exposing (..)
import Html.Attributes exposing (..)

termsPage : List (Html msg)
termsPage =
  [ div [ class "main content text" ]
    [ h1 []
      [ text "Terms of use" ]
    , p []
      [ text "Please do not do anything illegal or taboo with our service." ]
    ]
  ]

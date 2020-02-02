module Pages.Terms exposing (view)

import Html exposing (Html, text, div, h1, p)
import Html.Attributes exposing (class)

view : List (Html msg)
view =
  [ div [ class "main content text" ]
    [ h1 []
      [ text "Terms of use" ]
    , p []
      [ text "Please do not do anything illegal or taboo with our service." ]
    ]
  ]

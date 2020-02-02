module Pages.FrontPage exposing (view)

import Html exposing (Html, text, div, h1, p)
import Html.Attributes exposing (class)

view : List (Html msg)
view =
  [ div [ class "main content text" ]
    [ h1 []
      [ text "Front page" ]
    , p []
      [ text "I will add the front page later" ]
    ]
  ]

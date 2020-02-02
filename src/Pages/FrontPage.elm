module Pages.FrontPage exposing (view)

import Html exposing (..)
import Html.Attributes as A

view : List (Html msg)
view =
  [ div [ A.class "main content text" ]
    [ h1 []
      [ text "Front page" ]
    , p []
      [ text "I will add the front page later" ]
    ]
  ]

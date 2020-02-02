module Pages.Terms exposing (view)

import Html exposing (..)
import Html.Attributes as A

view : List (Html msg)
view =
  [ div [ A.class "main content text" ]
    [ h1 []
      [ text "Terms of use" ]
    , p []
      [ text "Please do not do anything illegal or taboo with our service." ]
    ]
  ]

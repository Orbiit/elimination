module Pages.About exposing (view)

import Html exposing (..)
import Html.Attributes as A

view : List (Html msg)
view =
  [ div [ A.class "main content text" ]
    [ h1 []
      [ text "About" ]
    , p []
      [ text "Yes..." ]
    ]
  ]

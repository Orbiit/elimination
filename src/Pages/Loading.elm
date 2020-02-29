module Pages.Loading exposing (view)

import Html exposing (..)
import Html.Attributes as A


view : List (Html msg)
view =
    [ article [ A.class "main content loading-wrapper" ]
        [ p [ A.class "loading-text" ]
            [ text "[temporary content]" ]
        ]
    ]

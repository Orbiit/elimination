module Pages.Error exposing (view)

import Html exposing (..)
import Html.Attributes as A
import Utils.Request as Request

view : Request.HttpError -> List (Html msg)
view (status, problem) =
  [ article [ A.class "main content http-error" ]
    [ div [ A.class "http-error-content" ]
      [ case status of
          Request.ErrorStatusText statusText ->
            h1 [ A.class "status-code is-text" ]
              [ text statusText ]
          Request.StatusCode statusCode ->
            h1 [ A.class "status-code" ]
              [ text (String.fromInt statusCode) ]
      , p [ A.class "problem" ]
        [ text problem ]
      , a [ A.href "?", A.class "button return-home" ]
        [ text "Return home" ]
      ]
    ]
  ]

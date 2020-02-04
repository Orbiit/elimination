module Pages exposing (..)

import Utils

type Page
  = FrontPage
  | Terms
  | Privacy
  | User
  | Game
  | UserSettings
  | GameSettings
  | Loading
  | Error Utils.HttpError
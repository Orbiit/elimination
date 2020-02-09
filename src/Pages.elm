module Pages exposing (..)

import Utils

type Page
  = FrontPage
  | Terms
  | Privacy
  | About
  | User
  | Game
  | UserSettings
  | GameSettings
  | Loading
  | Error Utils.HttpError
